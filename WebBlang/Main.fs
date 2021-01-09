module WebBlang.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client

open Blang

type Model =
    {
        Output: string list

        EvalEntryValue: string

        ReplState: RuntimeTypes.Scope
    }

let initModel =
    {
        Output = ["c"; "b"; "a"]
        EvalEntryValue = ""
        ReplState = RuntimeCore.coreScope
    }

type Message =
    // data setters
    | SetEvalEntryField of string

    // actions
    | EvalImmediate
    | ClearLog

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a

let private repl input state =
    Parser.parse (Lexer.create input, Lexer.next)
    <!> fun (value, rest) ->
            let parsedString = sprintf "> %s" (input.Substring(0, rest.Index))
            if rest |> Lexer.atEof |> not then
                (value, [
                    sprintf "WARNING: Input beyond the first value is currently discarded."
                    sprintf "         (Discarded input: \"%s\")" (rest.Source.Substring(rest.Index))
                    parsedString
                ])
            else
                value, [parsedString]
    >>= fun (value, msgs) -> 
            Runtime.evaluate state value <!> fun x -> x, msgs
    <!> fun ((value, scope), msgs) ->
            scope, msgs @ [Value.stringify value]

let update message model =
    match message with
    | SetEvalEntryField str ->
        { model with EvalEntryValue = str }

    | EvalImmediate ->
        if model.EvalEntryValue.Length > 0 then
            match repl model.EvalEntryValue model.ReplState with
            | Ok (nextState, msgs) ->
                { model with Output = model.Output @ [String.concat "\n" msgs]
                             ReplState = nextState
                             EvalEntryValue = "" }
            | Error err ->
                { model with Output = model.Output @ [sprintf "%A" err]
                             EvalEntryValue = "" }
        else
            model
    | ClearLog ->
        { model with Output = [] }

let view model dispatch =
    div [attr.``class`` "pb-2 pt-1 pl-2 pr-2 code-text"] [
        div [attr.``class`` "eval-log-container"] [
            table [attr.``class`` "m-0 table is-hoverable is-bordered is-narrow is-fullwidth scrolling-table"] [
                tbody [] [
                    forEach model.Output <| fun line ->
                        tr [] [
                            td [attr.``class`` "eval-log-entry"] [text line]
                        ]
                ]
            ]
        ]
        div [attr.id "eval-box-container"] [
            div [attr.``class`` "field pt-2 has-addons"] [
                div [attr.``class`` "control is-expanded"] [
                    input [attr.``class`` "input code-text is-dark"
                           attr.``type`` "text"
                           attr.``placeholder`` "Type expression and press Evaluate"
                           bind.input.string model.EvalEntryValue (dispatch << SetEvalEntryField)
                           on.keyup <| fun args -> match args.Key with
                                                   | "Enter" -> dispatch EvalImmediate
                                                   | _ -> ()]
                ]
                div [attr.``class`` "control"] [
                    button [
                        attr.``class`` "button is-dark is-outlined"
                        on.click <| fun _ -> dispatch EvalImmediate
                    ] [text "Evaluate"]
                ]
                div [attr.``class`` "control"] [
                    button [
                        attr.``class`` "button is-dark is-outlined"
                        on.click <| fun _ -> dispatch ClearLog
                    ] [text "Clear"]
                ]
            ]
        ]
    ]

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view

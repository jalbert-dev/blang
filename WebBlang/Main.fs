module WebBlang.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client

open Blang

type LogEntryType =
    | LogNone of string
    | LogWarning of string
    | LogError of string
    | LogInfo of string
    | LogEcho of string
    | LogResult of string

type Model =
    {
        Output: LogEntryType list

        EvalEntryValue: string

        ReplState: RuntimeTypes.Scope
    }

let initModel =
    {
        Output = []
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
            let parsedString = [LogEcho <| input.Substring(0, rest.Index)]
            if rest |> Lexer.atEof |> not then
                (value, [
                    LogWarning <| sprintf "Input beyond the first value is currently discarded.\n(Discarded input: \"%s\")" (rest.Source.Substring(rest.Index))
                ] @ parsedString)
            else
                value, parsedString
    >>= fun (value, msgs) -> 
            Runtime.evaluate state value <!> fun x -> x, msgs
    <!> fun ((value, scope), msgs) ->
            scope, msgs @ [LogResult <| Value.stringify value]

let update message model =
    match message with
    | SetEvalEntryField str ->
        { model with EvalEntryValue = str }

    | EvalImmediate ->
        if model.EvalEntryValue.Length > 0 then
            match repl model.EvalEntryValue model.ReplState with
            | Ok (nextState, msgs) ->
                { model with Output = model.Output @ msgs
                             ReplState = nextState
                             EvalEntryValue = "" }
            | Error err ->
                { model with Output = model.Output @ [LogError <| sprintf "%A" err]
                             EvalEntryValue = "" }
        else
            model
    | ClearLog ->
        { model with Output = [] }

let formatLogEntry (msg: LogEntryType) =
    let createEntry (headerClass, headerText) (entryClass, entryText) = [
        td [attr.``class`` ("eval-log-entry-type has-text-centered " + headerClass)] [text headerText]
        td [attr.``class`` ("eval-log-entry " + entryClass)] [text entryText]
    ]
    let createEntryWithIcon (headerClass, iconClass) (entryClass, entryText) = [
        td [attr.``class`` ("eval-log-entry-type has-text-centered " + headerClass)] 
            [i [attr.``class`` iconClass] []]
        td [attr.``class`` ("eval-log-entry " + entryClass)] [text entryText]
    ]
    tr [] (
        match msg with
        | LogEcho msg -> createEntry ("eval-log-entry-echo has-text-grey", "\ue0b1") ("eval-log-entry-echo has-text-grey", msg)
        | LogError msg -> createEntryWithIcon ("eval-log-entry-error has-background-danger-light", "fas fa-times has-text-danger") ("", msg)
        | LogInfo msg -> createEntryWithIcon ("eval-log-entry-info has-background-info-light", "fas fa-question has-text-info") ("", msg)
        | LogNone msg -> createEntry ("eval-log-entry-none", "") ("", msg)
        | LogResult msg -> createEntry ("eval-log-entry-result", "\u2570\u2500") ("has-text-weight-bold", msg)
        | LogWarning msg -> createEntryWithIcon ("eval-log-entry-warning has-background-warning-light", "fas fa-exclamation has-text-warning") ("", msg)
    )

let view model dispatch =
    div [attr.``class`` "pb-2 pt-1 pl-2 pr-2 code-text"] [
        div [attr.``class`` "eval-log-container sensible-font-size"] [
            table [attr.``class`` "m-0 table is-hoverable is-bordered is-narrow is-fullwidth scrolling-table"] [
                tbody [] [
                    forEach model.Output <| formatLogEntry
                ]
            ]
        ]
        div [attr.id "eval-box-container"] [
            div [attr.``class`` "field pt-2 has-addons"] [
                div [attr.``class`` "control is-expanded"] [
                    input [attr.``class`` "input code-text is-dark sensible-font-size"
                           attr.``type`` "text"
                           attr.``placeholder`` "Type expression and press Evaluate"
                           bind.input.string model.EvalEntryValue (dispatch << SetEvalEntryField)
                           on.keyup <| fun args -> match args.Key with
                                                   | "Enter" -> dispatch EvalImmediate
                                                   | _ -> ()]
                ]
                div [attr.``class`` "control"] [
                    button [
                        attr.``class`` "button is-dark is-outlined sensible-font-size"
                        on.click <| fun _ -> dispatch EvalImmediate
                    ] [text "Evaluate"]
                ]
                div [attr.``class`` "control"] [
                    button [
                        attr.``class`` "button is-dark is-outlined sensible-font-size"
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

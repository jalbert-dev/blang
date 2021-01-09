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

let update message model =
    match message with
    | SetEvalEntryField str ->
        { model with EvalEntryValue = str }

    | EvalImmediate ->
        if model.EvalEntryValue.Length > 0 then
            { model with Output = model.EvalEntryValue :: model.Output
                         EvalEntryValue = "" }
        else
            model
    | ClearLog ->
        { model with Output = [] }

let view model dispatch =
    div [attr.``class`` "pb-2 pt-1 pl-2 pr-2 code-text"] [
        table [attr.``class`` "m-0 table is-hoverable is-bordered is-narrow is-fullwidth"] [
            tbody [] [
                forEach (List.rev model.Output) <| fun line ->
                    tr [] [
                        td [] [text line]
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

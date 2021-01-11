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

        History: string list
        CurrentHistory: int

        EvalEntryValue: string

        ReplState: RuntimeTypes.Scope
    }

let initModel =
    {
        Output = []
        History = []
        CurrentHistory = -1
        EvalEntryValue = ""
        ReplState = RuntimeCore.coreScope
    }

type Message =
    // data setters
    | SetEvalEntryField of string

    // actions
    | EvalImmediate
    | ClearLog
    | HistoryBackward
    | HistoryForward

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a

let private repl input state =
    Parser.parse (Lexer.create input, Lexer.next)
    <!> fun (value, rest) ->
            value,
                if rest |> Lexer.atEof |> not then
                    [ LogWarning <| sprintf "Input beyond the first value is currently discarded.\n(Discarded input: \"%s\")" (rest.Source.Substring(rest.Index)) ]
                else
                    []
    >>= fun (value, msgs) -> 
            Runtime.evaluate state value <!> fun x -> x, msgs
    <!> fun ((value, scope), msgs) ->
            scope, msgs @ [LogResult <| Value.stringify value]

let setHistoryPosition pos model =
    if List.isEmpty model.History then
        model
    else
        { model with CurrentHistory = pos
                     EvalEntryValue = if pos >= 0 then
                                          List.item pos model.History
                                      else
                                          "" }

let update message model =
    match message with
    | SetEvalEntryField str ->
        { model with EvalEntryValue = str }

    | EvalImmediate ->
        if model.EvalEntryValue.Length > 0 then
            let echoMsg = LogEcho <| model.EvalEntryValue
            let model = { model with History = model.EvalEntryValue :: model.History
                                     CurrentHistory = -1 }
            match repl model.EvalEntryValue model.ReplState with
            | Ok (nextState, msgs) ->
                { model with Output = model.Output @ (echoMsg :: msgs)
                             ReplState = nextState
                             EvalEntryValue = "" }
            | Error err ->
                { model with Output = model.Output @ [echoMsg; LogError <| sprintf "%A" err]
                             EvalEntryValue = "" }
        else
            model
    | ClearLog ->
        { model with Output = [] }
    
    | HistoryBackward ->
        model |> (setHistoryPosition <| min (model.CurrentHistory + 1) (List.length model.History))
    | HistoryForward ->
        model |> (setHistoryPosition <| max (model.CurrentHistory - 1) -1)

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

let loadingText id displayText =
    div [attr.``id`` id; attr.``class`` "loading-text-parent"] [
        h1 [attr.``class`` "loading-text-centered"] [text displayText]
    ]

let view model dispatch =
    div [attr.``id`` "main"] [
        div [attr.``class`` "editor-container ctrl-bordered m-0"] [
            div [attr.``class`` "field has-addons m-0 p-0 is-fullwidth"] [
                p [attr.``class`` "control is-expanded"] [
                    div [attr.``class`` "select is-fullwidth sensible-font-size"] [
                        select [attr.``id`` "file-select-box"] [
                            option [] [text "test.blah"]
                            option [] [text "another.blah"]
                            option [] [text "fib.blah"]
                        ]
                    ]
                ]
                p [attr.``class`` "control"] [
                    button [
                        attr.``id`` "load-file-button"
                        attr.``class`` "button is-outlined is-dark sensible-font-size"
                    ] [text "Load"]
                ]
            ]

            div [attr.``class`` "container m-0 p-0 editor-parent"] [
                loadingText "editor-loading" "Loading editor..."
                textarea [attr.``id`` "editor-textarea"; attr.``style`` "display: none;"] []
            ]

            div [attr.``id`` "editor-fake-footer"
                 attr.``class`` "field has-addons is-fullwidth m-0 p-0 has-background-light"] [
                p [attr.``id`` "run-multiline-control"
                   attr.``class`` "control has-background-white"] [
                    button [
                        attr.``id`` "run-multiline-button"
                        attr.``class`` "button is-outlined is-dark sensible-font-size"
                    ] [text "Run"]
                ]
            ]
        ]
        div [attr.``class`` "repl-container m-0 code-text"] [
            div [attr.``class`` "ctrl-bordered eval-log-container sensible-font-size"] [
                div [attr.``class`` "fill-parent flex-backscroll"] [
                    table [attr.``class`` "m-0 table is-hoverable is-bordered is-narrow is-fullwidth scrolling-table ctrl-inside-bordered"] [
                        tbody [] [
                            forEach model.Output <| formatLogEntry
                        ]
                    ]
                ]
            ]
            div [attr.id "eval-box-container"] [
                div [attr.``class`` "field pt-2 has-addons"] [
                    div [attr.``class`` "control is-expanded"] [
                        input [attr.``id`` "repl-input"
                               attr.``class`` "input code-text is-dark sensible-font-size"
                               attr.``type`` "text"
                               attr.``placeholder`` "Type expression and press Evaluate"
                               bind.input.string model.EvalEntryValue (dispatch << SetEvalEntryField)
                               on.keyup <| fun args -> match args.Key with
                                                       | "Enter" -> dispatch EvalImmediate
                                                       | "ArrowUp" -> dispatch HistoryBackward
                                                       | "ArrowDown" -> dispatch HistoryForward
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
                            attr.``id`` "repl-clear-button"
                            attr.``class`` "button is-dark is-outlined sensible-font-size"
                            on.click <| fun _ -> dispatch ClearLog
                        ] [text "Clear"]
                    ]
                ]
            ]
        ]
    ]

open System.Threading.Tasks

type MyApp() =
    inherit ProgramComponent<Model, Message>()


    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
    
    override this.OnAfterRenderAsync (firstRender: bool) =
        let js = this.JSRuntime
        async {
            if firstRender then
                let! _ = js.InvokeAsync("createIde", [| |]).AsTask() |> Async.AwaitTask
                return ()
            else
                return ()
        } |> Async.StartAsTask :> Task

open System
open Blang

let printScope (scope: Blang.RuntimeTypes.Scope) =
    for x in scope.SymbolTable do
        printfn "            %s => %s" x.Key (Value.stringify x.Value)

let rec repl state =
    //printScope state
    Console.Write("> ")
    let input = Console.ReadLine()

    match Parser.parse (Lexer.create input, Lexer.next) with
    | Ok (value, rest) -> 
        if (not << Lexer.atEof) rest then
            Console.WriteLine("WARNING: Input beyond the first value is discarded.")
            printfn "         (Discarded input: \"%s\")" (rest.Source.Substring(rest.Index))
        match Runtime.evaluate state value with
        | Ok (value, newState) -> 
            printfn ""
            printfn "      result: %s" (Value.stringify value)
            printfn "    newState:"
            printScope newState
            printfn ""
            repl newState
        | Error err ->
            printfn "%A" err
    | Error err -> 
        printfn "%A" err
    
    repl state

let ( >>= ) a b = Result.bind b a
let rec evaluateAll state lexer =
    Parser.parse (lexer, Lexer.next)
    >>= fun (value, rest) ->
        Runtime.evaluate state value
        >>= fun (lastValue, scope) ->
            if Lexer.atEof rest then
                Ok (lastValue, scope)
            else
                evaluateAll scope rest
         
let runFile state path =
    System.IO.File.ReadAllText(path)
    |> Lexer.create
    |> evaluateAll state
    |> function
       | Ok (value, _) -> printfn "%s" (Value.stringify value)
       | Error err -> printfn "%A" err

[<EntryPoint>]
let main argv =
    printfn "Starting Blang interpreter!"
    match argv with
    | [| |] -> repl RuntimeCore.coreScope
    | [| path |] -> runFile RuntimeCore.coreScope path
    | _ -> printfn "Invalid args"
    0
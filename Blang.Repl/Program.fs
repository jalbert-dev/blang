open System
open Blang

let printScope (scope: Blang.RuntimeTypes.Scope) =
    for x in scope.SymbolTable do
        printfn "            %s => %s" x.Key (Value.stringify x.Value)

let rec repl state () =
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
            repl newState ()
        | Error err ->
            printfn "%A" err
    | Error err -> 
        printfn "%A" err
    
    repl state ()


[<EntryPoint>]
let main argv =
    repl RuntimeCore.coreScope ()
    0
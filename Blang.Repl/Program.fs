open System
open Blang

let rec repl () =
    Console.Write("> ")
    let input = Console.ReadLine()

    match Parser.parse (Lexer.create input, Lexer.next) with
    | Ok (value, rest) -> 
        if (not << Lexer.atEof) rest then
            Console.WriteLine("WARNING: Input beyond the first value is discarded.")
            printfn "         (Discarded input: \"%s\")" (rest.Source.Substring(rest.Index))
        printfn "%A" (Evaluator.evaluate (Scope.create None) value)
    | Error err -> printfn "%A" err

    repl ()

[<EntryPoint>]
let main argv =
    repl ()
    0
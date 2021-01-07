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
        match Runtime.evaluate (Scope.create None) value with
        | Ok value -> 
            printfn "%A" value
            printfn "%s" (Value.stringify value)
        | Error err ->
            printfn "%A" err
    | Error err -> printfn "%A" err

    repl ()

[<EntryPoint>]
let main argv =
    repl ()
    0
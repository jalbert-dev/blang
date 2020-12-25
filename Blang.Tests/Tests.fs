namespace Blang

module LexerTests =
    open Xunit
    open FsCheck.Xunit

    open Swensen.Unquote
    open Blang.Lexer

    // [<Fact>]
    // let ``Test stringification`` () =
    //     let sexpr =
    //         Expr [
    //             Symbol "+";
    //             Num 5.0;
    //             Expr [
    //                 Symbol "*";
    //                 Expr [
    //                     Symbol "fcall";
    //                     Num 2.0;
    //                     Num 3.0;
    //                     String "sauce";
    //                 ]
    //                 Num 3.0;
    //             ]
    //         ]

    //     test <@ sexpr |> Sexpr.stringify |> (=) "(+ 5 (* (fcall 2 3 \"sauce\") 3))" @>

    //     sexpr |> Sexpr.stringify |> printfn "%s"

    let checkOk = function | Ok _ -> true | _ -> false
    let checkError = function | Error _ -> true | Ok _ -> false
    let unwrapOk = function | Ok x -> x | _ -> failwith ""
    let unwrapError = function | Error x -> x | _ -> failwith ""

    let expectToken value line character lexer =
        let result = lexer |> next
        test <@ checkOk result @>
        let (token, rest) = unwrapOk result
        test <@ token.Type = value @>
        test <@ token.Position = { Line = line; Character = character } @>
        rest
    
    let expectError error line character lexer =
        let result = lexer |> next
        test <@ checkError result @>
        test <@ unwrapError result = { Type = error; Position = { Line = line; Character = character } } @>

    [<Fact>] 
    let ``empty lex is always EOF`` () =
        create "" |> expectToken EOF 1 1
    
    [<Fact>] 
    let ``multiple lex at EOF always returns an EOF without moving lexer`` () =
        create "" |> expectToken EOF 1 1 |> expectToken EOF 1 1 |> expectToken EOF 1 1

    [<Fact>]
    let ``reserved symbols lex to their token equivalents`` () =
        let expect str tokType = str |> create |> expectToken tokType 1 1 |> expectToken EOF 1 2 |> ignore
        expect "'" SingleQuote
        expect "(" LParen
        expect ")" RParen
    
    [<Fact>]
    let ``whitespace is ignored`` () =
        create "    \t  \n \r\n\t" |> expectToken EOF 3 2
    
    [<Fact>]
    let ``comment character causes rest of line to be ignored`` () =
        create "# this is a comment line (+ 3 4 '(hello 34))\n" |> expectToken EOF 2 1

    [<Fact>]
    let ``empty string lex to empty string token`` () =
        create "\"\"" |> expectToken (String "") 1 1 |> expectToken EOF 1 3
    
    [<Fact>]
    let ``unterminated string lexes to error`` () =
        create "\"" |> expectError UnterminatedString 1 1

    [<Fact>]
    let ``simple string lexes to corresponding string token`` () =
        create "\"simple string\"" |> expectToken (String "simple string") 1 1 |> expectToken EOF 1 16

    [<Fact>]
    let ``multiple string + whitespace lex`` () =
        create "  \"hurr\" \n\t\t\n\t\"durf\"\t\"hyur\""
        |> expectToken (String "hurr") 1 3
        |> expectToken (String "durf") 3 2
        |> expectToken (String "hyur") 3 9
        |> expectToken EOF 3 15

    // [<Property>]
    // let ``parse of number must result in Num with parsed value`` (x: double) =
    //     let str = sprintf "%g" x
    //     test <@ Sexpr.parse str = Num x @>
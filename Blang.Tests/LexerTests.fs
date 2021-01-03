module Blang.Tests.Lexer

open Xunit
open FsCheck
open Swensen.Unquote
open Blang.Lexer
open Blang.ParserTypes

let checkOk = function | Ok _ -> true | _ -> false
let checkError = function | Error _ -> true | Ok _ -> false
let unwrapOk = function | Ok x -> x | _ -> failwith ""
let unwrapError = function | Error x -> x | _ -> failwith ""

let expectOkLex lexer =
    let result = lexer |> next
    test <@ checkOk result @>
    unwrapOk result

let expectErrorLex lexer =
    let result = lexer |> next
    test <@ checkError result @>
    unwrapError result

let expectToken expected line character lexer =
    let (token, rest) = expectOkLex lexer
    test <@ token.Type = expected @>
    test <@ token.Position = { Line = line; Character = character } @>
    rest

let expectApproxNum expected lexer =
    let (token, rest) = expectOkLex lexer
    test <@ match token.Type with 
            | Number actual -> (actual - expected |> abs) < 0.0001
            | _ -> false @>
    rest

let expectString expected lexer =
    let (token, rest) = expectOkLex lexer
    test <@ match token.Type with
            | String actual -> actual = expected 
            | _ -> false @>
    rest

let expectSymbol expected lexer =
    let (token, rest) = expectOkLex lexer
    test <@ match token.Type with
            | Symbol actual -> actual = expected 
            | _ -> false @>
    rest

let expectTokenType expected lexer =
    let (token, rest) = expectOkLex lexer
    test <@ token.Type = expected @>
    rest

let expectError error line character lexer =
    let result = expectErrorLex lexer
    test <@ result = { Type = error; Position = { Line = line; Character = character } } @>

let expectErrorType errorType lexer =
    let err = expectErrorLex lexer
    test <@ errorType = err.Type @>

let expectPosition line character (lexer: LexState) =
    test <@ lexer.Position = { Line = line; Character = character } @>

let [<Fact>] ``empty lex is always EOF`` () =
    create "" |> expectToken EOF 1 1

let [<Fact>] ``multiple lex at EOF always returns an EOF without moving lexer`` () =
    create "" |> expectToken EOF 1 1 |> expectToken EOF 1 1 |> expectToken EOF 1 1

let [<Fact>] ``reserved symbols lex to their token equivalents`` () =
    let expect str tokType = str |> create |> expectToken tokType 1 1 |> expectToken EOF 1 2 |> ignore
    expect "(" LParen
    expect ")" RParen

let [<Fact>] ``whitespace is ignored`` () =
    create "    \t  \n \r\n\t" |> expectToken EOF 3 2

let [<Fact>] ``comment character causes rest of line to be ignored`` () =
    create "# this is a comment line (+ 3 4 '(hello 34))\n" |> expectToken EOF 2 1

let [<Fact>] ``empty string lex to empty string token`` () =
    create "\"\"" |> expectToken (String "") 1 1 |> expectToken EOF 1 3

let [<Fact>] ``unterminated string lexes to error`` () =
    create "\"" |> expectError UnterminatedString 1 1

let [<Fact>] ``simple string lexes to corresponding string token`` () =
    create "\"simple string\"" |> expectToken (String "simple string") 1 1 |> expectToken EOF 1 16

let [<Fact>] ``multiple string and whitespace lex`` () =
    create "  \"hurr\" \n\t\t\n\t\"durf\"\t\"hyur\""
    |> expectToken (String "hurr") 1 3
    |> expectToken (String "durf") 3 2
    |> expectToken (String "hyur") 3 9
    |> expectToken EOF 3 15

let [<Fact>] ``EOF when lexing symbol is symbol completion`` () =
    create "define-symbol" |> expectToken (Symbol "define-symbol") 1 1 |> expectToken EOF 1 14

[<Theory>]
[<InlineData(" ", 1, 15)>]
[<InlineData("\t", 1, 15)>]
[<InlineData("\r", 1, 15)>]
[<InlineData("\n", 2, 1)>]
[<InlineData("\r\n", 2, 1)>]
let ``whitespace when lexing symbol is symbol completion`` delimiter line column =
    sprintf "define-symbol%sanother-symbol" delimiter
    |> create
    |> expectToken (Symbol "define-symbol") 1 1
    |> expectToken (Symbol "another-symbol") line column
    |> expectToken EOF line (column + 14)

[<Theory>]
[<InlineData("(")>]
[<InlineData(")")>]
[<InlineData("\"")>]
let ``symbol completion char when lexing symbol is symbol completion`` str =
    sprintf "define-symbol%s" str
    |> create
    |> expectToken (Symbol "define-symbol") 1 1
    |> expectPosition 1 14

let [<Fact>] ``arbitrary numbers and most special characters are allowed in symbols`` () =
    create "special!-symbol1.2.3.4550@$,G0"
    |> expectToken (Symbol "special!-symbol1.2.3.4550@$,G0") 1 1

[<Theory>]
[<InlineData("あぁ　アァ　（・ω・）　ののの")>]
[<InlineData("私は日本語を話すのが下手です")>]
[<InlineData("🤔🦑")>]
[<InlineData("\uD83E\uDD14\uD83E\uDD91-this-is-actually-identical-to-the-last-one")>]
let ``unicode symbol names are allowed and are lexed properly`` str =
    create str |> expectToken (Symbol str) 1 1 |> expectToken EOF 1 (1 + str.Length)

[<Theory>]
[<InlineData("あぁ　アァ　（・ω・）　ののの")>]
[<InlineData("私は日本語を話すのが下手です")>]
[<InlineData("🤔🦑 emoji are weird, y'all （・ω・）")>]
[<InlineData("\uD83E\uDD14\uD83E\uDD91 yup, those are utf16 encoded as ascii. what a world")>]
let ``unicode strings are allowed and are lexed properly`` str =
    str |> sprintf "\"%s\"" |> create |> expectToken (String str) 1 1 |> expectToken EOF 1 (1 + str.Length + 2)

let [<Fact>] ``multiline strings are allowed and are lexed literally`` () =
    create "\"hello. this is dog?\nhello dog. this is not dog.\noh no.\""
    |> expectToken (String "hello. this is dog?\nhello dog. this is not dog.\noh no.") 1 1
    |> expectToken EOF 3 8

let [<Fact>] ``floating-pointing values lex as numbers`` =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.Default.NormalFloat())
            (fun (NormalFloat x) -> 
                sprintf "%g" x
                |> create
                |> expectApproxNum x
                |> ignore))

let [<Fact>] ``standalone minus lexes as symbol`` () =
    create "  -  "
    |> expectToken (Symbol "-") 1 3
    |> expectToken EOF 1 6

let [<Fact>] ``minus before digit lexes as number`` () =
    create "  -2 "
    |> expectApproxNum -2.0

let [<Fact>] ``minus dot is invalid number and lineinfo points to numerical part`` () =
    create "-."
    |> expectError InvalidNumber 1 2

let [<Fact>] ``solitary dot is invalid number`` () =
    create "."
    |> expectError InvalidNumber 1 1

let [<Fact>] ``dot digit is valid number`` () =
    create ".5"
    |> expectApproxNum 0.5

let [<Fact>] ``negative dot digit is valid number`` () =
    create "-.5"
    |> expectApproxNum -0.5

let [<Fact>] ``digit dot is valid number`` () =
    create "1."
    |> expectApproxNum 1.0

let [<Fact>] ``negative digit dot is valid number`` () =
    create "-1."
    |> expectApproxNum -1.0

[<Theory>]
[<InlineData("1..0", 1, 3)>]
[<InlineData("1.0.", 1, 4)>]
[<InlineData("1.0.0", 1, 4)>]
[<InlineData("..1.0", 1, 2)>]
[<InlineData(".1.0", 1, 3)>]
[<InlineData("-.1.0", 1, 4)>]
[<InlineData("-..1.0", 1, 3)>]
let ``multiple decimal points is unexpected character`` str line column =
    create str
    |> expectError (UnexpectedCharacter '.') line column

let fuzzStringCharaSet =
    Gen.elements (seq {
        for c in ' '..'!' -> string c
        for c in '#'..'~' -> string c
        for c in [| '\t'; '\r'; '\n'; '\n'; '\n'; '\n'; '\n'; '\n' |] -> string c
        for i in [0x1F913..0x1F937] -> System.Char.ConvertFromUtf32(i)
        for i in [0x3041..0x3060] -> System.Char.ConvertFromUtf32(i)
    })
let fuzzSymbolCharaSet =
    fuzzStringCharaSet 
    |> Gen.filter (fun str ->
        not <| List.contains str [
            " "; "\r"; "\t"; "\n";
            "."; "("; ")"; "\""; "#";
        ])
let fuzzSymbolStarterCharaSet =
    fuzzSymbolCharaSet
    |> Gen.filter (fun str ->
        not <| Seq.contains str (seq { for c in '0'..'9' -> string c }))

let fuzzStringGen minLength maxLength =
    gen {
        let! length = Gen.choose(minLength, maxLength)
        let! fuzzString = Gen.arrayOfLength length fuzzStringCharaSet
        return fuzzString |> String.concat ""
    }
let fuzzStringArb minLength maxLength = fuzzStringGen minLength maxLength |> Arb.fromGen

// note that a symbol must have at least one character, unlike strings.
// so length determines number of trailing characters instead
// also aggravating: leading minus is allowed in symbol names, but if that
// minus is there, a numeric digit immediately after will result in a number parse!
// so the fuzzer will reject digits in 2nd spot if first char is '-'
let fuzzSymbolGen minLength maxLength =
    let noWackyDigits (str: string) = str.[0] < '0' || str.[0] > '9'
    gen {
        let! length = Gen.choose(minLength, maxLength)
        let! startChar = fuzzSymbolStarterCharaSet
        let! secondChar = 
            if startChar = "-" then
                Gen.filter noWackyDigits fuzzSymbolCharaSet
            else
                fuzzSymbolCharaSet
        let! remaining = Gen.listOfLength (length - 1) fuzzSymbolCharaSet
        return (startChar :: secondChar :: remaining) |> String.concat ""
    }
let fuzzSymbolArb minLength maxLength = fuzzSymbolGen minLength maxLength |> Arb.fromGen

let [<Fact>] ``any unicode string not containing unescaped double quotes is valid`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (fuzzStringArb 0 1200)
            (fun str ->
                sprintf "\"%s\"" str
                |> create
                |> expectString str
                |> ignore))

let [<Fact>] ``any unicode symbol name not containing whitespace or reserved characters is valid`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (fuzzSymbolArb 0 96)
            (fun str ->
                create str
                |> expectSymbol str
                |> ignore))

let [<Fact>] ``symbol on adjacent right of number lexes as number followed by symbol`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.fromGen 
                (gen {
                    let! (NormalFloat num) = Arb.Default.NormalFloat().Generator
                    let! sym = fuzzSymbolGen 0 24
                    return (sprintf "%g%s" num sym, num, sym)
                }))
            (fun (str, expectedNum, expectedSym) ->
                create str
                |> expectApproxNum expectedNum
                |> expectSymbol expectedSym
                |> ignore))

let [<Fact>] ``number on adjacent right of symbol lexes as symbol`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.fromGen 
                (gen {
                    let! (NormalFloat num) = Arb.Default.NormalFloat().Generator
                    let! sym = fuzzSymbolGen 0 24
                    return sprintf "%s%g" sym num
                }))
            (fun str ->
                create str
                |> expectSymbol str
                |> ignore))

let [<Fact>] ``string on adjacent right of symbol lexes as symbol followed by string`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.fromGen 
                (gen {
                    let! str = fuzzStringGen 0 120
                    let! sym = fuzzSymbolGen 0 24
                    return (sprintf "%s\"%s\"" sym str, str, sym)
                }))
            (fun (str, expectedStr, expectedSym) ->
                create str
                |> expectSymbol expectedSym
                |> expectString expectedStr
                |> ignore))

let [<Fact>] ``symbol on adjacent right of string lexes as string followed by symbol`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.fromGen 
                (gen {
                    let! str = fuzzStringGen 0 120
                    let! sym = fuzzSymbolGen 0 24
                    return (sprintf "\"%s\"%s" str sym, str, sym)
                }))
            (fun (str, expectedStr, expectedSym) ->
                create str
                |> expectString expectedStr
                |> expectSymbol expectedSym
                |> ignore))

let [<Fact>] ``number on adjacent right of string lexes as string followed by number`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.fromGen 
                (gen {
                    let! (NormalFloat num) = Arb.Default.NormalFloat().Generator
                    let! str = fuzzStringGen 0 120
                    return (sprintf "\"%s\"%g" str num, num, str)
                }))
            (fun (str, expectedNum, expectedStr) ->
                create str
                |> expectString expectedStr
                |> expectApproxNum expectedNum
                |> ignore))

let [<Fact>] ``string on adjacent right of number lexes as number followed by string`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (Arb.fromGen 
                (gen {
                    let! (NormalFloat num) = Arb.Default.NormalFloat().Generator
                    let! str = fuzzStringGen 0 120
                    return (sprintf "%g\"%s\"" num str, num, str)
                }))
            (fun (str, expectedNum, expectedStr) ->
                create str
                |> expectApproxNum expectedNum
                |> expectString expectedStr
                |> ignore))

let [<Fact>] ``comment on adjacent right to symbol is ignored`` () =
    create "symbol#this is a comment!"
    |> expectSymbol "symbol"
    |> expectTokenType EOF

let [<Fact>] ``comment on adjacent right to string is ignored`` () =
    create "\"string\"#this is a comment!"
    |> expectString "string"
    |> expectTokenType EOF

let [<Fact>] ``comment on adjacent right to number is ignored`` () =
    create "1.305#this is a comment!"
    |> expectApproxNum 1.305
    |> expectTokenType EOF

[<Theory>]
[<InlineData("\"This is a string\"")>]
[<InlineData("(")>]
[<InlineData(")")>]
[<InlineData("# comments shouldn't matter but what the hey! test cases are free")>]
let ``minus on adjacent left of non-number lexes as symbol`` str =
    sprintf "-%s" str |> create |> expectSymbol "-"

let [<Fact>] ``minus on adjacent left of symbol is part of that symbol`` () =
    create "-merged-symbol!" |> expectSymbol "-merged-symbol!"

let [<Fact>] ``negative number on adjacent left of symbol lexes as number then symbol`` () =
    create "-9Wv0m" |> expectApproxNum -9.0 |> expectSymbol "Wv0m"

let [<Fact>] ``period followed by symbol is invalid number`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (fuzzSymbolArb 0 24)
            (fun str ->
                sprintf ".%s" str
                |> create
                |> expectErrorType InvalidNumber
                |> ignore))

let [<Fact>] ``period followed by string is invalid number`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll
            (fuzzStringArb 0 24)
            (fun str ->
                sprintf ".\"%s\"" str
                |> create
                |> expectErrorType InvalidNumber
                |> ignore))

[<Theory>]
[<InlineData("(")>]
[<InlineData(")")>]
let ``period followed by reserved character is invalid number`` str =
    sprintf ".%s" str
    |> create
    |> expectErrorType InvalidNumber

let [<Fact>] ``consecutive periods is unexpected character`` () =
    create "..." |> expectErrorType (UnexpectedCharacter '.')

let [<Fact>] ``adjacent strings lex as multiple strings`` () =
    Check.QuickThrowOnFailure
        (Prop.forAll 
            (fuzzStringGen 0 24 |> Gen.two |> Arb.fromGen)
            (fun (a, b) ->
                sprintf "\"%s\"\"%s\"" a b
                |> create
                |> expectString a
                |> expectString b
                |> ignore))

let [<Fact>] ``test case: simple s-expression`` () =
    create "(bind-function fib (' n) -5.01 (\"don't forget me\"))"
    |> expectTokenType LParen
    |> expectSymbol "bind-function"
    |> expectSymbol "fib"
    |> expectTokenType LParen
    |> expectSymbol "'"
    |> expectSymbol "n"
    |> expectTokenType RParen
    |> expectApproxNum -5.01
    |> expectTokenType LParen
    |> expectString "don't forget me"
    |> expectTokenType RParen
    |> expectTokenType RParen
    |> expectTokenType EOF

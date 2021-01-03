module Blang.Tests.Parser

open Xunit
open Swensen.Unquote

open Blang.Parser
open Blang.ParserTypes
open Blang.RuntimeTypes
open Blang.ErrorTypes

// The parser interface is designed to avoid coupling to a specific token source.
// For the purposes of unit testing, we'll create a token source that just iterates
// through a fixed list of tokens.
let createToken t =
    { Token.Type = t;
      Position = 
        { Line = 0; Character = 0 } }
let getNext = function
    | [] -> Ok (createToken EOF, [])
    | h::t -> Ok (createToken h, t)
let tokenSourceFromList lst =
    (lst, getNext)

// Since our test tokens will propagate their position to the runtime values that
// they end up parsing to, we need to make sure the expected output has that same
// position of 0:0, rather than None. (A little bit of a hack, I guess!)
let createValueAt00 valueType =
    { Value.Type = valueType 
      Position =
        Some { Line = 0; Character = 0 } }
let Expression = Expression >> createValueAt00
let NumberAtom = NumberAtom >> createValueAt00
let StringAtom = StringAtom >> createValueAt00
let SymbolAtom = SymbolAtom >> createValueAt00

let expectParseValue tokenList (expectedValue: Value) =
    test <@ match tokenList |> (tokenSourceFromList >> parse) with
            | Ok (actual, []) -> actual.Type = expectedValue.Type
            | _ -> false @>
let expectParseResult tokenList (expectedValue: Value, expectedTail) =
    test <@ match tokenList |> (tokenSourceFromList >> parse) with
            | Ok (actual, rest) -> actual.Type = expectedValue.Type && rest = expectedTail 
            | _ -> false @>
let expectParseError tokenList expectedError =
    test <@ match tokenList |> (tokenSourceFromList >> parse) with
            | Error actual -> actual.Type = expectedError 
            | _ -> false @>

let ( => ) = expectParseValue
let ( =>& ) = expectParseResult
let ( =/> ) = expectParseError

let [<Fact>] ``empty parse returns unit expression value`` () =
    [] => unitValue

let [<Fact>] ``parsing lone rparen returns unexpected token`` () =
    [RParen] =/> UnexpectedToken RParen

let [<Fact>] ``parsing lone lparen returns error expected value`` () =
    [LParen] =/> ExpectedValue

let [<Fact>] ``parsing lone number returns that number`` () =
    [Number 5.0] => NumberAtom 5.0

let [<Fact>] ``parsing lone string returns that string`` () =
    [String "yup"] => StringAtom "yup"

let [<Fact>] ``parsing lone symbol returns that symbol`` () =
    [Symbol "sym"] => SymbolAtom "sym"

let [<Fact>] ``parsing lparen-rparen returns empty expression`` () =
    [LParen; RParen] => Expression []

let [<Fact>] ``parse call only consumes one value from input`` () =
    [Symbol "sym1"; Symbol "sym2"] =>& ((SymbolAtom "sym1"), [Symbol "sym2"])

let [<Fact>] ``parse of unclosed expression containing value is error expected value`` () =
    [LParen; Symbol "sburb"] =/> ExpectedValue

let [<Fact>] ``parse of expression containing single atom`` () =
    [LParen; Symbol "borptis"; RParen] => Expression [SymbolAtom "borptis"]

let [<Fact>] ``parse of expression containing multiple atoms`` () =
    [LParen; Symbol "bempty"; Number 1.0; String "nope"; RParen]
    =>
    Expression [SymbolAtom "bempty"; NumberAtom 1.0; StringAtom "nope"]

let [<Fact>] ``parse of expression starting with subexpr`` () =
    [LParen; LParen; Symbol "inside!"; RParen; Symbol "outside!"; RParen]
    =>
    Expression [
        Expression [
            SymbolAtom "inside!" ];
        SymbolAtom "outside!"]

let [<Fact>] ``parse of expression with middle subexpr`` () =
    [LParen; Symbol "first"; LParen; Symbol "middle"; RParen; Symbol "last"; RParen]
    =>
    Expression [
        SymbolAtom "first";
        Expression [
            SymbolAtom "middle"];
        SymbolAtom "last"]

let [<Fact>] ``parse of expression with ending subexpr`` () =
    [LParen; Symbol "first"; LParen; Symbol "last"; RParen; RParen]
    =>
    Expression [
        SymbolAtom "first";
        Expression [
            SymbolAtom "last"]]

let [<Fact>] ``parse of multiple nested subexprs`` () =
    [LParen;
        LParen;
            Symbol "1";
            LParen;
                Symbol "2";
            RParen;
        RParen;
        LParen;
            Symbol "3";
            Symbol "4";
        RParen;
    RParen]
    =>
    Expression [
        Expression [
            SymbolAtom "1";
            Expression [
                SymbolAtom "2"]]
        Expression [
            SymbolAtom "3";
            SymbolAtom "4"]]

// Checking that long expressions don't overflow the stack is kind of
// pointless in Debug configuration, since AFAIK tail call optimizations
// are only applied in Release. So we'll just remove it entirely
#if !DEBUG

// The length of a list shouldn't be limited by the stack, i.e. parsing at the
// same depth level should be tail recursive, so we'll throw a million at the test
let [<Literal>] MAX_TEST_BREADTH = 1000000
// Depth is necessarily not tail recursive, so we're limited by stack space,
// but at the same time, there's no reasonable justification for supporting
// eg. nested expressions 10,000 levels deep, so no need to go that far
let [<Literal>] MAX_TEST_DEPTH = 2000

let [<Fact>] ``stack overflow check: parse of very long list`` () =
    let input = 
        seq {
            yield LParen
            for i in 1..MAX_TEST_BREADTH -> Number (i |> double)
            yield RParen
        } |> Seq.toList
    let expected =
        Expression
            (seq {
                for i in 1..MAX_TEST_BREADTH -> NumberAtom (i |> double)
            } |> Seq.toList)
    input => expected

let [<Fact>] ``stack overflow check: parse of deeply nested expressions`` () =
    let input =
        seq {
            for i in 1..MAX_TEST_DEPTH -> LParen
            yield Number 1.0
            for i in 1..MAX_TEST_DEPTH -> RParen
        } |> Seq.toList
    let mutable expected = NumberAtom 1.0
    for i in 1..MAX_TEST_DEPTH do
        expected <- Expression [expected]
    input => expected

#endif
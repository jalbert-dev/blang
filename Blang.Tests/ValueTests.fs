module Blang.Tests.Value

open Xunit
open Swensen.Unquote

open Blang.RuntimeTypes
open Blang.Value

// The three current atom value types are trivially stringified
// and details are subject to change, so for the moment I'm only 
// going to bother testing a few simple expressions.

let [<Fact>] ``a symbol value stringifies to its value`` () =
    test <@ stringify (SymbolAtom "hello-world") = "hello-world" @>

let [<Fact>] ``a string value stringifies to its value surrounded by double quotes`` () =
    test <@ stringify (StringAtom "hello world") = "\"hello world\"" @>

let [<Fact>] ``an empty expression stringifies to pair of parens`` () =
    test <@ stringify (Expression []) = "()" @>

let [<Fact>] ``an expression only containing a number has no whitespace when stringified`` () =
    test <@ stringify (Expression [NumberAtom -1.234]) |> Seq.contains ' ' |> not @>

let [<Fact>] ``an expression only containing a string is the string bordered by parens`` () =
    test <@ stringify (Expression [StringAtom "hello!"]) = "(\"hello!\")" @>

let [<Fact>] ``an expression only containing a symbol is the symbol bordered by parens`` () =
    test <@ stringify (Expression [SymbolAtom "hello!"]) = "(hello!)" @>

let [<Fact>] ``an expression containing multiple values separates them with a space`` () =
    test <@ stringify (Expression [SymbolAtom "sym1"; SymbolAtom "sym2"]) = "(sym1 sym2)" @>

let [<Fact>] ``an expression containing an expression recursively stringifies`` () =
    test <@ stringify (Expression [Expression [SymbolAtom "hi"]]) = "((hi))" @>

let [<Fact>] ``test case: simple expression`` () =
    test <@ stringify (
                Expression [
                    Expression [
                        SymbolAtom "x";
                        SymbolAtom "y";
                    ];
                    Expression [
                        SymbolAtom "+";
                        SymbolAtom "x";
                        SymbolAtom "y";
                    ];
                ])
                = "((x y) (+ x y))" @>
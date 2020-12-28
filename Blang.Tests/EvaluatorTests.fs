module Blang.Tests.Evaluator

open Xunit
open Swensen.Unquote

open Blang.Evaluator
open Blang.RuntimeTypes

// TODO: right now these tests rely on the lexer component. better to make an ITokenSource
//       and make a concrete version that wraps the lexer, then another to facilitate testing via raw token stream

let getTestState = Blang.Lexer.create

let [<Fact>] ``parsing squote-lparen-rparen returns empty expression`` () =
    let result = evalStep (getTestState "'()") (createScope None)
    test <@ match result with
            | Ok (Expression [], _, _) -> true
            | _ -> false @>
module Blang.Tests.Evaluator

open Xunit
open Swensen.Unquote
open Blang
open Blang.RuntimeTypes
open Blang.ErrorTypes

let ( <!> ) a b = Result.map b a
let ( >>= ) a b = Result.bind b a

let NumVal = NumberAtom >> Value.createAnon
let SymVal = SymbolAtom >> Value.createAnon
let StrVal = StringAtom >> Value.createAnon
let ExprVal = Expression >> Value.createAnon

let ErrorEvaluatingFunction (name, errType) =
    ErrorEvaluatingFunction (name, { EvalError.Type = errType; Position = None})

let funcCallExpr (name, args) =
    ExprVal (SymVal name :: args)
let quotedValue (value) =
    funcCallExpr ("'", [value])
let bindValueExpr (name, value) =
    funcCallExpr ("bind-value", [quotedValue (SymVal name); value])
let makeFunctionExpr (args, body) =
    funcCallExpr ("make-function", [
        quotedValue (ExprVal (List.map SymVal args))
        quotedValue (ExprVal body) ])
let bindFunctionExpr (name, args, body) =
    funcCallExpr ("bind-function", [
        quotedValue (SymVal name)
        quotedValue (ExprVal (List.map SymVal args))
        quotedValue (ExprVal body) ])

let makeUserFunction (args, body) =
    ExprVal [StrVal "u"
             ExprVal (List.map SymVal args)
             ExprVal body]

let unwrapOk = function | Ok x -> x | _ -> failwith ""

let expectSuccessWithResult expected result =
    test <@ result = Ok expected @>
    unwrapOk result

let setUpEval bindings input =
    let mutable scope = RuntimeCore.coreScope
    for (name, value) in bindings do
        scope <- { scope with SymbolTable = scope.SymbolTable.Add (name, value) }
    Runtime.evaluate scope input

let ( .=>. ) (bindings, input) (expectedValue, expectedBindings) =
    match setUpEval bindings input with
    | Ok (value, scope) ->
        test <@ value = expectedValue @>
        for (expectedName, expectedValue) in expectedBindings do
            match expectedValue with
            | { Value.Type = StringAtom "{NOT-SET}" } -> 
                test <@ Map.containsKey expectedName scope.SymbolTable |> not @>
            | _ -> 
                test <@ Map.tryFind expectedName scope.SymbolTable = Some expectedValue @>
    | Error err ->
        Assert.True(false, sprintf "Unexpected evaluation failure: %A" err)
    
let ( .=> ) input expected = input .=>. (expected, [])
let ( =>. ) input expected = ([], input) .=>. expected
let ( => ) input expected = ([], input) .=>. (expected, [])

let ( =/> ) input expectedError =
    match setUpEval [] input with
    | Ok x ->
        Assert.True(false, sprintf "Unexpected evaluation success: %A" x)
    | Error err ->
        test <@ err.Type = expectedError @>


let [<Fact>] ``number value evaluates to itself`` () =
    NumVal 5.0 => NumVal 5.0

let [<Fact>] ``string value evaluates to itself`` () =
    StrVal "Whatever" => StrVal "Whatever"

let [<Fact>] ``symbol value evaluates to bound value in scope`` () =
    (["symbol-name", NumVal 1.2], SymVal "symbol-name") .=> NumVal 1.2

let [<Fact>] ``symbol without corresponding bound value fails to evaluate`` () =
    SymVal "hello-i'm-invalid" =/> UnboundIdentifier "hello-i'm-invalid"

let [<Fact>] ``first value in an expression must evaluate to a symbol`` () =
    ExprVal [StrVal "nope!"] =/> InvalidFunctionDefinition (StringAtom "nope!")
    ExprVal [NumVal 1.0] =/> InvalidFunctionDefinition (NumberAtom 1.0)

let [<Fact>] ``test case: simple function invocation`` () =
    ExprVal [SymVal "+"; NumVal 1.0; NumVal 5.0] => NumVal 6.0

let [<Fact>] ``test case: nested invocation`` () =
    ExprVal [ SymVal "+"
              NumVal 3.0
              ExprVal [ SymVal "*"
                        NumVal 6.0
                        NumVal 4.0 ]]
    => NumVal 27.0

let [<Fact>] ``quote function returns its argument unevaluated`` () =
    quotedValue(StrVal "hello!") => StrVal "hello!"
    quotedValue(NumVal 1.2) => NumVal 1.2
    quotedValue(SymVal "a-symbol") => SymVal "a-symbol"
    quotedValue(funcCallExpr ("+", [NumVal 2.0; SymVal "wubba"]))
    => funcCallExpr (("+"), [NumVal 2.0; SymVal "wubba"])

let [<Fact>] ``list function returns a list containing its evaluated arguments`` () =
    funcCallExpr ("[]", [
        NumVal 1.0
        StrVal "value!"
        bindValueExpr ("xvalue", NumVal 3.0)
        SymVal "xvalue"
        quotedValue (SymVal "xvalue")
        funcCallExpr ("+", [NumVal 1.0; NumVal 4.0])
        quotedValue(funcCallExpr ("+", [NumVal 1.0; NumVal 2.0]))
    ])
    => ExprVal [
        NumVal 1.0
        StrVal "value!"
        NumVal 3.0
        NumVal 3.0
        SymVal "xvalue"
        NumVal 5.0
        funcCallExpr ("+", [NumVal 1.0; NumVal 2.0])
    ]

let [<Fact>] ``bind-value alters its caller's scope and returns the value bound`` () =
    bindValueExpr ("x", StrVal "value") =>. (StrVal "value", ["x", StrVal "value"])

let [<Fact>] ``function arguments evaluate from left to right`` () =
    funcCallExpr ("+", [
        bindValueExpr ("x", NumVal 3.0)
        SymVal "x"
    ])
    => NumVal 6.0

    funcCallExpr ("+", [
        SymVal "x"
        bindValueExpr ("x", NumVal 3.0)
    ])
    =/> ErrorEvaluatingFunction ("+", UnboundIdentifier "x")

let [<Fact>] ``eval function passes its evaluated argument through the evaluator again`` () =
    funcCallExpr ("eval", [quotedValue (funcCallExpr ("+", [NumVal -2.0; NumVal 3.0]))])
    => NumVal 1.0

let [<Fact>] ``make-function returns a callable function`` () =
    ExprVal [
        makeFunctionExpr (["x"; "y"], [funcCallExpr ("+", [SymVal "x"; SymVal "y"])])
        NumVal 2.0
        NumVal 5.0]
    => NumVal 7.0

let [<Fact>] ``user functions with zero arguments evaluate correctly`` () =
    ExprVal [makeFunctionExpr ([], [funcCallExpr ("+", [NumVal 2.0; NumVal 3.0])])]
    => NumVal 5.0

let [<Fact>] ``supplying too few arguments to a native function evaluates to an error`` () =
    funcCallExpr ("+", [NumVal 0.0])
    =/> ErrorEvaluatingFunction ("+", WrongNumberOfSuppliedArguments (1, 2))

let [<Fact>] ``supplying too many arguments to a native function evaluates to an error`` () =
    funcCallExpr ("+", [NumVal 0.0; NumVal 1.0; NumVal 2.0])
    =/> ErrorEvaluatingFunction ("+", WrongNumberOfSuppliedArguments (3, 2))

let [<Fact>] ``supplying too few arguments to a user function evaluates to an error`` () =
    ExprVal [
        makeFunctionExpr (["x"; "y"], [funcCallExpr ("+", [SymVal "x"; SymVal "y"])])
        NumVal 2.0]
    =/> ErrorEvaluatingFunction (Evaluator.ANONYMOUS_FUNCTION_STACKTRACE, WrongNumberOfSuppliedArguments (1, 2))

let [<Fact>] ``supplying too many arguments to a user function evaluates to an error`` () =
    ExprVal [
        makeFunctionExpr (["x"; "y"], [funcCallExpr ("+", [SymVal "x"; SymVal "y"])])
        NumVal 2.0
        NumVal 3.0
        NumVal 4.0]
    =/> ErrorEvaluatingFunction (Evaluator.ANONYMOUS_FUNCTION_STACKTRACE, WrongNumberOfSuppliedArguments (3, 2))

let [<Fact>] ``attempting to call native function with invalid name evaluates to an error`` () =
    ExprVal [ quotedValue(ExprVal [StrVal "n"; StrVal "spicy"]) ]
    =/> ErrorEvaluatingFunction (Evaluator.ANONYMOUS_FUNCTION_STACKTRACE, InvalidNativeFunctionName "spicy")

let [<Fact>] ``user functions return the value of their final expression`` () =
    ExprVal [
        makeFunctionExpr (["x"; "y"], [
            funcCallExpr ("+", [SymVal "x"; SymVal "y"])
            funcCallExpr ("*", [SymVal "x"; SymVal "y"])
            StrVal "Just kidding!"
        ])
        NumVal 2.0
        NumVal 5.0]
    => StrVal "Just kidding!"

let [<Fact>] ``bind-function creates a function bound to a name in the caller's scope`` () =
    let body = [funcCallExpr("+", [SymVal "x"; SymVal "y"])]
    let expected = makeUserFunction (["x"; "y"], body)
    bindFunctionExpr ("f", ["x"; "y"], body)
    =>. (expected, ["f", expected])

let [<Fact>] ``bind-value in tail position must not leak outside the scope of its caller`` () =
    (["f", makeUserFunction(["x"; "y"], [bindValueExpr ("result", funcCallExpr("+", [SymVal "x"; SymVal "y"]))])],
     funcCallExpr("f", [NumVal -2.0; NumVal 4.5]))
    .=>. (NumVal 2.5, ["result", StrVal "{NOT-SET}"])

#if !DEBUG
// as with the parser, stack overflow tests are only run in release config,
// since tail call optimization is disabled by default in debug config

[<Literal>]
let TAIL_CALL_TEST_DEPTH = 200000

let [<Fact>] ``user functions are tail call optimized in release mode`` () =
    (["recurse", makeUserFunction(["i"; "n"], [
        funcCallExpr ("eval", [
            funcCallExpr ("if", [
                funcCallExpr ("<", [SymVal "i"; SymVal "n"])
                quotedValue (funcCallExpr ("recurse", [
                    funcCallExpr("+", [SymVal "i"; NumVal 1.0])
                    SymVal "n" ]))
                Parser.unitValue ])
        ])
    ])], funcCallExpr ("recurse", [NumVal 0.0; NumVal (double TAIL_CALL_TEST_DEPTH)]))
    .=> Parser.unitValue

#endif
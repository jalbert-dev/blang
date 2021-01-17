module Blang.Tests.Evaluator

open Xunit
open Swensen.Unquote
open Blang
open Blang.RuntimeTypes

let ( <!> ) a b = Result.map b a
let ( >>= ) a b = Result.bind b a

let NumberVal = NumberAtom >> Value.createAnon
let SymbolVal = SymbolAtom >> Value.createAnon
let StringVal = StringAtom >> Value.createAnon
let ExprVal = Expression >> Value.createAnon

let unwrapOk = function | Ok x -> x | _ -> failwith ""

let expectSuccessWithResult expected result =
    test <@ result = Ok expected @>
    unwrapOk result

let ( .=>. ) (bindings, input) (expectedValue, expectedBindings) =
    let mutable scope = Scope.create None
    for (name, value) in bindings do
        scope <- { scope with SymbolTable = scope.SymbolTable.Add (name, value) }
    match Runtime.evaluate scope input with
    | Ok (value, scope) ->
        test <@ value = expectedValue @>
        for (expectedName, expectedValue) in expectedBindings do
            test <@ Map.tryFind expectedName scope.SymbolTable = Some expectedValue @>
    | Error err ->
        let msg = sprintf "Unexpected evaluation failure: %A" err
        Assert.True(false, msg)
    
let ( .=> ) input expected = input .=>. (expected, [])
let ( =>. ) input expected = ([], input) .=>. expected
let ( => ) input expected = ([], input) .=>. (expected, [])

let [<Fact>] ``number value evaluates to itself`` () =
    NumberVal 5.0 => NumberVal 5.0

let [<Fact>] ``string value evaluates to itself`` () =
    StringVal "Whatever" => StringVal "Whatever"

let [<Fact>] ``symbol value evaluates to bound value in scope`` () =
    (["symbol-name", NumberVal 1.2], SymbolVal "symbol-name") .=> NumberVal 1.2
module Blang.EvalUtil

open Blang
open Blang.ErrorTypes
open Blang.RuntimeTypes

let expectArgsLength expectedLen args =
    let len = List.length args
    if len <> expectedLen then
        Error (len, expectedLen)
    else
        Ok args

let unwrapAtomSymbol = function
    | { Value.Type = SymbolAtom x } -> x
    | _ -> failwith "Type checking failure!"

let unwrapAtomNum = function
    | { Value.Type = NumberAtom x } -> x
    | _ -> failwith "Type checking failure!"

let unwrapExpr = function
    | { Value.Type = Expression x } -> x
    | _ -> failwith "Type checking failure!"

let expectNumber (value: Value) =
    match value.Type with
    | NumberAtom x -> Ok x
    | _ -> Error { EvalError.Type = ExpectedNumber value.Type
                   Position = value.Position }

let expectSymbol (value: Value) =
    match value.Type with
    | SymbolAtom x -> Ok x
    | _ -> Error { EvalError.Type = ExpectedSymbol value.Type
                   Position = value.Position }

let expectString (value: Value) =
    match value.Type with
    | StringAtom x -> Ok x
    | _ -> Error { EvalError.Type = ExpectedString value.Type
                   Position = value.Position }

let expectExpression (value: Value) =
    match value.Type with
    | Expression x -> Ok x
    | _ -> Error { EvalError.Type = ExpectedExpression value.Type
                   Position = value.Position }

// applicatives are fancier but not as straightforwardly tail recursive (at my skill level, anyhow), 
// and this compiles down to a simple while loop meaning my stack won't blow up. sorry FP!
let rec invertResultList acc f = function
    | [] -> Ok (List.rev acc)
    | h::t ->
        match (f h) with
        | Ok value -> invertResultList (value :: acc) f t
        | Error err -> Error err

let lookupSymbolValue scope = function
    | { Value.Type = SymbolAtom sym; Position = pos } ->
        // lookup value bound to sym in scope
        match Scope.lookupValue sym scope with
        | Some value -> Ok value
        | None -> Error { EvalError.Type = UnboundIdentifier sym 
                          Position = pos }
    | whatever -> Ok whatever
module Blang.Evaluator

open Blang.RuntimeTypes
open Blang.ErrorTypes
open Blang.EvalUtil

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a

let private createFunctionScope scope args = args, Scope.create (Some scope)

let private expectFunctionIdentifier value =
    expectSymbol value >>! fun _ -> { EvalError.Type = ExpectedNumber value.Type
                                      Position = value.Position }

let wrapErrorInFunction fname pos err =
    { EvalError.Type = ErrorEvaluatingFunction (fname, err)
      Position = pos }

let rec evaluate (scope: Scope) (value: Value) : Result<Value, EvalError> =
    match value.Type with
    | Expression [] -> Ok Parser.unitValue
    | Expression (funcIdent::args) ->
        evaluate scope funcIdent
        >>= expectFunctionIdentifier
        >>= fun identifier ->
            // evaluate each argument in the list of arguments
            args |> (evaluate scope |> invertResultList [])
            // substitute any remaining symbols with the values they bind
            >>= (lookupSymbolValue scope |> invertResultList [])
            >>= fun args ->
                    //>>= bindArgs // type-check and bind args to new eval scope if a single-bind function
                    match Runtime.Core.functionMap.TryGetValue identifier with
                    | true, f -> f args (Scope.create (Some scope))
                                 >>! wrapErrorInFunction identifier funcIdent.Position
                    | _ -> Error { ErrorTypes.EvalError.Type = UnboundIdentifier identifier 
                                   Position = funcIdent.Position }
    | _ -> Ok value
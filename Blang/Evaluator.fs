module Blang.Evaluator

open Blang.RuntimeTypes
open Blang.ErrorTypes
open Blang.EvalUtil

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a

type NativeFunc = (Value -> Result<Value, EvalError>) -> Scope -> Value list -> Result<Value, EvalError>
type NativeFuncMap = Map<string, NativeFunc>

let private createFunctionScope scope args = args, Scope.create (Some scope)

let private expectFunctionIdentifier value =
    expectSymbol value >>! fun _ -> { EvalError.Type = FunctionIdentifierMustBeSymbol value.Type
                                      Position = value.Position }

let wrapErrorInFunction fname pos err =
    { EvalError.Type = ErrorEvaluatingFunction (fname, err)
      Position = pos }

let rec evaluate (nativeFuncs: NativeFuncMap) (scope: Scope) (value: Value) : Result<Value, EvalError> =
    let evaluate = evaluate nativeFuncs scope
    match value.Type with
    | Expression [] -> Ok Parser.unitValue
    | Expression (funcIdent::args) ->
        evaluate funcIdent
        >>= expectFunctionIdentifier
        >>= fun identifier ->
                match nativeFuncs.TryGetValue identifier with
                | true, f -> 
                    args
                    |> f evaluate (Scope.create (Some scope))
                    >>! wrapErrorInFunction identifier funcIdent.Position
                | _ -> Error { ErrorTypes.EvalError.Type = UnboundIdentifier identifier 
                               Position = funcIdent.Position }
                //>>= bindArgs // type-check and bind args to new eval scope if a single-bind function
    | _ -> Ok value
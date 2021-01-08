module Blang.Evaluator

open Blang.RuntimeTypes
open Blang.ErrorTypes
open Blang.EvalUtil

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a

type NativeFunc = (Scope -> Value -> Result<Value * SideEffect list, EvalError>) -> Scope -> Value list -> Result<Value * SideEffect list, EvalError>
type NativeFuncMap = Map<string, NativeFunc>

let (|ExprValueType|_|) = function
    | { Value.Type = Expression x } -> Some x
    | _ -> None
let (|StringValueType|_|) = function
    | { Value.Type = StringAtom x } -> Some x
    | _ -> None

let (|NativeFuncValue|_|) = function
    | ExprValueType [StringValueType "n"; StringValueType funcId] -> Some funcId
    | _ -> None

let private createFunctionScope scope args = args, Scope.create (Some scope)

let private expectFunctionIdentifier value =
    expectSymbol value >>! fun _ -> { EvalError.Type = FunctionIdentifierMustBeSymbol value.Type
                                      Position = value.Position }

let wrapErrorInFunction fname pos err =
    { EvalError.Type = ErrorEvaluatingFunction (fname, err)
      Position = pos }

let rec applySideEffects scope = function
    | [] -> scope
    | h::t -> match h with
              | BindLocalValue (name, value) -> 
                    applySideEffects { scope with SymbolTable = scope.SymbolTable |> Map.add name value } t
let rec private propagateSideEffects scope (value, sideEffects) =
    value, (applySideEffects scope sideEffects)
let rec private evalAndPropagateSideEffects evaluator scope acc = function
    | [] -> Ok (List.rev acc, scope)
    | h::t -> 
        evaluator scope h 
        <!> propagateSideEffects scope
        >>= fun (value, scope) ->
            evalAndPropagateSideEffects evaluator scope (value :: acc) t

let private mapTuple (f, g) (a, b) = (f a, g b)

let trace x = printfn "%A" x; x

let rec evaluate
                (nativeFuncs: NativeFuncMap) 
                (scope: Scope) 
                (value: Value) 
                : Result<Value * SideEffect list, EvalError> =
    let eval' = evaluate nativeFuncs
    // `scope` is the scope that is to be modified by SideEffects.
    match value.Type with
    // Expressions need to be evaluated, Symbols need to be looked up, everything else passes through
    | SymbolAtom _ -> lookupSymbolValue scope value <!> fun x -> x, []
    | Expression [] -> Ok (Parser.unitValue, [])
    | Expression (funcIdent::args) ->
        eval' scope funcIdent
        <!> mapTuple (id, applySideEffects scope)
        >>= fun (value, scope) ->
                match value with
                | NativeFuncValue identifier ->
                    match nativeFuncs.TryGetValue identifier with
                    | true, f ->
                        let evalEnv =
                            if identifier = "'" || identifier = ":" then
                                Ok (args, scope)
                            else
                                evalAndPropagateSideEffects eval' scope [] args

                        evalEnv >>= fun (args, scope) -> f eval' scope args
                    | _ -> Error { ErrorTypes.EvalError.Type = UnboundIdentifier identifier 
                                   Position = funcIdent.Position }
                | _ -> Error { EvalError.Type = InvalidFunctionDefinition value.Type; Position = None}
    | _ -> Ok (value, [])

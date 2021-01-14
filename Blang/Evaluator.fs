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

let (|NativeFuncDef|_|) = function
    | ExprValueType [StringValueType NATIVEFUNC_SIG; StringValueType funcId] -> Some funcId
    | _ -> None

let private expectFunctionIdentifier value =
    expectSymbol value >>! fun _ -> { EvalError.Type = FunctionIdentifierMustBeSymbol value.Type
                                      Position = value.Position }

let wrapFuncEvalError fname pos err =
    { EvalError.Type = ErrorEvaluatingFunction (fname, err)
      Position = pos }

let applySideEffect scope = function
    | BindLocalValue (name, value) -> scope |> Scope.bindValueTo name value
let applySideEffects = List.fold applySideEffect

let private lookupNativeFunc (nativeFuncs: NativeFuncMap) tokenPosition name =
    match nativeFuncs.TryGetValue name with
    | true, f -> Ok f
    | _ -> Error { EvalError.Type = InvalidNativeFunctionName name
                   Position = tokenPosition }

// Applies a pair of functions to a pair of corresponding values.
let private mapPair (f, g) (a, b) = (f a, g b)
let private prependTo b a = a :: b

let ( >=> ) f g x = f x >>= g

// Evaluates a list of values sequentially within the given scope, updating the scope according
// to any side effects returned while evaluating each. Returns a Result containing
// either a tuple of the list of evaluated values and the final scope, or an EvalError.
let private evaluateValueListWithSideEffects evaluator startScope valueList =
    let f value (acc, scope) = evaluator scope value <!> mapPair (prependTo acc, applySideEffects scope)
    // side effects must be applied in list order
    List.fold (fun s v -> s >>= f v) <| Ok ([], startScope) <| valueList
    // that means the result list will be backwards due to prepend/cons!
    <!> mapPair (List.rev, id)

// Evaluates a given value inside the given scope and applies any side effects to
// the scope. Returns a Result containing either a tuple of the evaluated value
// and the updated scope, or an EvalError.
let private evaluateValueSingleWithSideEffects evaluator startScope value =
    evaluateValueListWithSideEffects evaluator startScope [value]
    <!> mapPair (List.exactlyOne, id)

// Helper function to parse function definitions and run the corresponding executors.
let private evaluateFunctionWithDef (nativeFuncs: NativeFuncMap)
                                    evaluator
                                    functionStartPosition
                                    functionIdentifierPosition
                                    args 
                                    (funcDef, evaluationScope) =
    let evaluator = evaluator nativeFuncs
    match funcDef with
    | NativeFuncDef identifier ->
        let evalArgs () =
            // For now, quote function is special-cased to not evaluate its args
            if identifier = "'" then
                Ok (args, evaluationScope)
            else
                evaluateValueListWithSideEffects evaluator evaluationScope args

        let execNativeFunc f =
            evalArgs ()
            >>= fun (args, scope) -> f evaluator scope args
            >>! wrapFuncEvalError identifier functionStartPosition
        
        identifier
        |> lookupNativeFunc nativeFuncs functionIdentifierPosition
        >>= execNativeFunc
    | _ -> Error { EvalError.Type = InvalidFunctionDefinition funcDef.Type
                   Position = None}

/// Evaluates the given value within the given scope, and returns a Result
/// containing either a tuple of the resulting value and a list of side effects,
/// or an EvalError.
/// 
/// Returned SideEffects are to be applied by the caller.
let rec evaluateValue (nativeFuncs: NativeFuncMap) 
                 (scope: Scope) 
                 (value: Value) 
                 : Result<Value * SideEffect list, EvalError> =
    // Expressions need to be evaluated as a function, Symbols need to be looked up, everything else passes through
    match value.Type with
    | SymbolAtom _ -> lookupSymbolValue scope value <!> fun x -> x, []
    | Expression [] -> Ok (Parser.unitValue, [])
    | Expression (funcIdent::args) ->
        let evaluateFunctionWithDef' = evaluateFunctionWithDef nativeFuncs evaluateValue value.Position
        let evaluateValueSingleWithSideEffects' = evaluateValueSingleWithSideEffects (evaluateValue nativeFuncs) scope

        evaluateValueSingleWithSideEffects' funcIdent
        >>= evaluateFunctionWithDef' funcIdent.Position args
    | _ -> Ok (value, [])

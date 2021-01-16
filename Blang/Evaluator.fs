module Blang.Evaluator

open Blang.RuntimeTypes
open Blang.ErrorTypes
open Blang.EvalUtil

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a
let private ( <*> ) f x = 
    match (f, x) with
    | Ok f, Ok x -> Ok <| f x
    | Error err, _ -> Error err
    | _, Error err -> Error err

type NativeFunc = Scope -> Value list -> Result<ValueDef * SideEffect list, EvalError>
type NativeFuncMap = Map<string, NativeFunc>

type private ExecDef =
    | NativeCall of NativeFunc * Value list * Scope
    | UserCall of Value list * Scope

let (|ExprValueType|_|) = function
    | { Value.Type = Expression x } -> Some x
    | _ -> None
let (|StringValueType|_|) = function
    | { Value.Type = StringAtom x } -> Some x
    | _ -> None

let (|NativeFuncDef|UserFuncDef|InvalidFuncDef|) = function
    | ExprValueType [StringValueType NATIVEFUNC_SIG; StringValueType funcId] -> NativeFuncDef funcId
    | ExprValueType [StringValueType USERFUNC_SIG
                     ExprValueType paramNames
                     ExprValueType funcBody] -> UserFuncDef (paramNames, funcBody)
    | _ -> InvalidFuncDef


let private expectFunctionIdentifier value =
    expectSymbol value >>! fun _ -> { EvalError.Type = FunctionIdentifierMustBeSymbol value.Type
                                      Position = value.Position }

let private wrapFuncEvalError fname pos err =
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
let private evaluateValueListWithSideEffects evaluator stackTrace startScope valueList =
    let f value (acc, scope) = evaluator stackTrace scope value <!> mapPair (prependTo acc, applySideEffects scope)
    // side effects must be applied in list order
    List.fold (fun s v -> s >>= f v) <| Ok ([], startScope) <| valueList
    // that means the result list will be backwards due to prepend/cons!
    <!> mapPair (List.rev, id)

// Evaluates a given value inside the given scope and applies any side effects to
// the scope. Returns a Result containing either a tuple of the evaluated value
// and the updated scope, or an EvalError.
let private evaluateValueSingleWithSideEffects evaluator stackTrace startScope value =
    evaluateValueListWithSideEffects evaluator stackTrace startScope [value]
    <!> mapPair (List.exactlyOne, id)

// Helper function to parse function definitions and return any
// information needed to perform the function call.
let private prepareFuncCall nativeFuncLookup
                            evaluator
                            stackTrace
                            args 
                            (funcDef, evaluationScope) =
    let ( <!> ) = Result.map

    match funcDef with
    | NativeFuncDef identifier ->
        let evalArgs () =
            // For now, quote function is special-cased to not evaluate its args
            if identifier = "'" then
                Ok (args, evaluationScope)
            else
                evaluateValueListWithSideEffects evaluator stackTrace evaluationScope args
        
        fun f (args, scope) -> NativeCall (f, args, scope)
        <!> nativeFuncLookup identifier
        <*> evalArgs ()
    
    | UserFuncDef (paramNames, funcBody) ->
        let ( <!!> ) f = Result.bind <| fun (a, b) -> Ok (f a b)

        let bindArgsToNames args scope names =
            List.zip names args
            |> List.map BindLocalValue
            |> List.fold applySideEffect scope

        let expectSymbolList values = invertResultList [] expectSymbol values

        let evalScope () =
            bindArgsToNames 
            <!!> evaluateValueListWithSideEffects evaluator stackTrace evaluationScope args
            <*> (expectSymbolList paramNames >>! fun x -> x, stackTrace)

        let expectedNumArgs = List.length paramNames
        let actualNumArgs = List.length args
        if expectedNumArgs <> actualNumArgs then
            Error ({ EvalError.Type = WrongNumberOfSuppliedArguments (actualNumArgs, expectedNumArgs)
                     Position = None }, stackTrace)
        else
            fun a b -> UserCall (a, b)
            <!> (funcBody |> Ok)
            <*> evalScope ()

    | InvalidFuncDef -> Error ({ EvalError.Type = InvalidFunctionDefinition funcDef.Type
                                 Position = None}, [])

/// Evaluates the given value within the given scope, and returns a Result
/// containing either a tuple of the resulting value and a list of side effects,
/// or an EvalError.
/// 
/// Returned SideEffects are to be applied by the caller.
let evaluateValue (nativeFuncs: NativeFuncMap)
                  (rootScope: Scope)
                  (rootValue: Value)
                  : Result<Value * SideEffect list, EvalError> =
    let rec loop allowReturnedSideEffects stackTrace scope (value: Value) =
        // Expressions need to be evaluated as a function, Symbols need to be looked up, everything else passes through
        match value.Type with
        | SymbolAtom _ -> lookupSymbolValue scope value >>! (fun x -> x, stackTrace) <!> fun x -> x, []
        | Expression [] -> Ok (Parser.unitValue, [])
        | Expression (funcIdent::args) ->
            let stackTrace = funcIdent :: stackTrace
            let nativeFuncLookup x = lookupNativeFunc nativeFuncs value.Position x >>! (fun x -> x, stackTrace)
            let prepareFuncCall' = prepareFuncCall nativeFuncLookup (loop true) stackTrace
            let evaluateValueSingleWithSideEffects' = evaluateValueSingleWithSideEffects (loop true) stackTrace scope

            evaluateValueSingleWithSideEffects' funcIdent
            >>= prepareFuncCall' args
            >>= function
                | NativeCall (f, evaledArgs, scope) -> 
                    f scope evaledArgs >>! (fun x -> x, stackTrace)
                    <!> fun (value, sideEffects) -> 
                            if not allowReturnedSideEffects then
                                value, []
                            else
                                value, sideEffects
                | UserCall (functionBody, scope) ->
                    let ( <!> ) = Result.map
                    let rec evalFunctionBody scope = function
                        | [] -> (Immediate Parser.unitValue, []) |> Ok
                        | [lastExpr] -> (NeedsUserEval (lastExpr, scope), []) |> Ok
                        | h::t ->
                            evaluateValueSingleWithSideEffects (loop true) stackTrace scope h
                            >>= fun (_, newScope) -> evalFunctionBody newScope t
                    evalFunctionBody scope functionBody
            >>= function
                | (Immediate value, sideEffects) -> Ok (value, sideEffects)
                | (NeedsEval (toEval, scope), []) -> loop true stackTrace scope toEval
                | (NeedsUserEval (toEval, scope), []) -> loop false stackTrace scope toEval
                | _ -> failwith "Functions can't return values needing evaluation AND side effects!!"
        | _ -> Ok (value, [])
    let flattenStackTrace (terminalError, stackTrace) =
        let getStackInfo = function
            | { Value.Type = StringAtom x; Position = y } -> x, y
            | { Value.Type = SymbolAtom x; Position = y } -> x, y
            | { Value.Type = Expression _; Position = y } -> "[anonymous function expression]", y
            | { Position = y } -> "[invalid function definition]", y
        let mutable rv = terminalError
        for err in stackTrace do
            let (msg, pos) = getStackInfo err
            rv <- { EvalError.Type = ErrorEvaluatingFunction (msg, rv)
                    Position = pos }
        rv

    loop true [] rootScope rootValue 
    >>! flattenStackTrace

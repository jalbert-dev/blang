module Blang.RuntimeCore

open Blang
open Blang.ErrorTypes
open Blang.RuntimeTypes
open Blang.EvalUtil

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a
let private ( >=> ) f g arg = f arg >>= g

let private isNumber x = expectNumber x <!> ignore
let private isString x = expectString x <!> ignore
let private isSymbol x = expectSymbol x <!> ignore
let private isExpr x = expectExpression x <!> ignore

let private expectArgList typeCheckList args =
    let expectedLen = List.length typeCheckList
    let actualLen = List.length args
    if actualLen <> expectedLen then
        Error { EvalError.Type = WrongNumberOfSuppliedArguments (actualLen, expectedLen)
                Position = None }
    else
        // zip list of args with type check list
        List.zip typeCheckList args
        // then again we bind type checks to args and invert the result list
        |> invertResultList [] (fun (typeCheck, arg) -> typeCheck arg)
        >>= fun _ -> Ok args

let private evalAndLookupArgs evaluator lookup args =
    // evaluate each argument in the list of arguments
    // and lookup their value if symbol
    args |> ((evaluator >=> lookup) |> invertResultList [])

let private prepareArgsOfType evaluator lookup typeCheckList args =
    args
    |> evalAndLookupArgs evaluator lookup
    >>= expectArgList typeCheckList

let private wrapBinaryOp op eval scope (args: Value list) =
    args |> prepareArgsOfType eval (lookupSymbolValue scope) [
        isNumber;
        isNumber ]
    <!> fun args -> 
            (op (unwrapAtomNum args.[0]) (unwrapAtomNum args.[1]) |> NumberAtom |> Value.createAnon)

let private quote _ _ args = 
    args |> Expression |> Value.createAnon |> Ok
let private list evaluator (scope: Scope) args =
    args |> evalAndLookupArgs evaluator (lookupSymbolValue scope) <!> (Expression >> Value.createAnon)
let private eval evaluator scope args =
    args |> prepareArgsOfType evaluator (lookupSymbolValue scope) [ isExpr ]
    >>= fun args -> evaluator args.[0]

let functionMap : Evaluator.NativeFuncMap =
    Map <| seq {
        yield "'", quote
        yield "[]", list
        yield "eval", eval
        
        yield "+", wrapBinaryOp ( + );
        yield "-", wrapBinaryOp ( - );
        yield "*", wrapBinaryOp ( * );
        yield "/", wrapBinaryOp ( / );
        yield "mod", wrapBinaryOp ( % );
    }
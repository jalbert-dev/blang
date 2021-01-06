module Blang.Runtime.Core

open Blang
open Blang.ErrorTypes
open Blang.RuntimeTypes
open Blang.EvalUtil

type private NativeFunc = Value list -> Scope -> Result<Value, EvalError>
type private NativeFuncMap = Map<string, NativeFunc>

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a

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

let private wrapBinaryOp op (args: Value list) _ =
    args |> expectArgList [
        isNumber;
        isNumber ]
    <!> fun args -> 
            (op (unwrapAtomNum args.[0]) (unwrapAtomNum args.[1]) |> NumberAtom |> Value.createAnon)

let functionMap : NativeFuncMap =
    Map <| seq {
        yield "+", wrapBinaryOp ( + );
        yield "-", wrapBinaryOp ( - );
        yield "*", wrapBinaryOp ( * );
        yield "/", wrapBinaryOp ( / );
        yield "mod", wrapBinaryOp ( % );
    }
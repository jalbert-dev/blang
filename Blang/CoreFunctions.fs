module Blang.RuntimeCore

open Blang
open Blang.ErrorTypes
open Blang.RuntimeTypes
open Blang.EvalUtil

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a
let private ( >>! ) a b = Result.mapError b a
let private ( >=> ) f g arg = f arg >>= g

let private boolToResult err = function
    | true  -> Ok ()
    | false -> Error err

let private isNumber x = expectNumber x <!> ignore
let private isString x = expectString x <!> ignore
let private isSymbol x = expectSymbol x <!> ignore
let private isExpr x = expectExpression x <!> ignore
let private isQuoteExpr = function
    | { Value.Type = Expression (h::_) } when h.Type = SymbolAtom "'" -> Ok ()
    | _ -> Error { EvalError.Type = ExpectedValue; Position = None }
let private isHomogeneousList predicate err = 
    let rec loop = function
        | [] -> Ok ()
        | h::t ->
            h |> predicate >>! err >>= fun _ -> loop t
    expectExpression >=> loop
let private isAnyValue _ = Ok ()

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

let private wrapBinaryOp op _ (args: Value list) =
    args |> expectArgList [
        isNumber
        isNumber ]
    <!> fun args -> 
            op (unwrapAtomNum args.[0]) (unwrapAtomNum args.[1]) 
            |> NumberAtom 
            |> Value.createAnon
            |> Immediate, []
let private wrapComparison op = wrapBinaryOp (fun x y -> op x y |> function | true -> 1.0 | false -> 0.0)

let private numberEquals _ (args: Value list) =
    args |> expectArgList [
        isNumber
        isNumber ]
    <!> fun args ->
            NumberAtom (if abs ((unwrapAtomNum args.[0]) - (unwrapAtomNum args.[1])) < 0.0000001  then
                            1.0
                        else
                            0.0) |> Value.createAnon |> Immediate, []
                    

let private unwrapQuote = function
    | { Value.Type = Expression (h::t) } when h.Type = SymbolAtom "'" -> Ok (Expression t)
    | _ -> Error { EvalError.Type = ExpectedValue; Position = None }

let withNoSideEffects f x = (f x, [])

let private quote _ args =
    args |> expectArgList [ isAnyValue ] <!> (List.head >> Immediate >> withNoSideEffects id)
let private list _ =
    Expression >> Value.createAnon >> Immediate >> withNoSideEffects id >> Ok
let private eval scope args =
    args |> expectArgList [ isAnyValue ]
    <!> fun args -> NeedsEval (args.[0], scope), []

let private ifExpression _ args =
    args |> expectArgList [ isNumber; isAnyValue; isAnyValue ]
    <!> fun args ->
        let whichBranch =
            if (unwrapAtomNum args.[0] > 0.0000001) then
                1
            else
                2
        Immediate args.[whichBranch], []

let private bindValue _ args =
    args |> expectArgList [
        isSymbol
        isAnyValue ]
    <!> fun args ->
            (Immediate args.[1], [BindLocalValue (unwrapAtomSymbol args.[0], args.[1])])

let private makeFunction _ args =
    args |> expectArgList [
        isHomogeneousList isSymbol id
        isExpr ]
    >>= fun args ->
            list () (Value.createAnon (StringAtom USERFUNC_SIG) :: args)

let private bindFunction _ args =
    args |> expectArgList [
        isSymbol
        isHomogeneousList isSymbol id
        isExpr ]
    >>= fun args ->
            // pass the latter two args to makeFunction
            makeFunction () args.[1..]
    // then let bindValue bind the resulting Blang function to the given name
    >>= function
        | (Immediate funcExpr, _) -> bindValue () [ args.[0]; funcExpr ]
        | _ -> failwith ""

let functionMap : Evaluator.NativeFuncMap =
    Map <| seq {
        yield "'", quote
        yield "[]", list
        yield "eval", eval
        yield "make-function", makeFunction
        yield "bind-value", bindValue
        yield "bind-function", bindFunction
        
        yield "<", wrapComparison ( < );
        yield "<=", wrapComparison ( <= );
        yield ">", wrapComparison ( > );
        yield ">=", wrapComparison ( >= );

        yield "+", wrapBinaryOp ( + );
        yield "-", wrapBinaryOp ( - );
        yield "*", wrapBinaryOp ( * );
        yield "/", wrapBinaryOp ( / );
        yield "mod", wrapBinaryOp ( % );
        yield "=", numberEquals

        yield "if", ifExpression
    }

let coreScope : Scope =
    let nativeFunctionBinding id _ =
        Expression [
            StringAtom NATIVEFUNC_SIG |> Value.createAnon
            StringAtom id |> Value.createAnon] 
        |> Value.createAnon
    { Scope.create None with SymbolTable = functionMap
                                           |> Map.map nativeFunctionBinding }
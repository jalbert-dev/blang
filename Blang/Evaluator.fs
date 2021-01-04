module Blang.Evaluator

open Blang.RuntimeTypes
open Blang.ErrorTypes

let createScope (parent: Scope option) =
    { EnclosingScope = parent
      SymbolTable = Map.empty }

let ( >>= ) a b = Result.bind b a
let ( <!> ) a b = Result.map b a
let ( >>! ) a b = Result.mapError b a

let private expectSymbol (value: Value) =
    match value.Type with
    | SymbolAtom name -> Ok name
    | _ -> Error { ErrorTypes.EvalError.Type = FunctionIdentifierMustBeSymbol value.Type
                   Position = value.Position }

let private expectArgsLength expectedLen args =
    let len = List.length args
    if len <> expectedLen then
        Error (len, expectedLen)
    else
        Ok args

let private unwrapAtomNum = function
    | { Value.Type = NumberAtom x } -> x
    | _ -> failwith "Type checking failure!"

let private lookupSymbolValue scope = function
    | { Value.Type = SymbolAtom sym; Position = pos } ->
        // lookup value bound to sym in scope
        match Scope.lookupValue sym scope with
        | Some value -> Ok value
        | None -> Error { ErrorTypes.EvalError.Type = UnboundIdentifier sym 
                          Position = pos }
    | whatever -> Ok whatever

let rec lookupSymbolsInArgList scope lst = function
    | [] -> Ok (List.rev lst)
    | h::t ->
        match lookupSymbolValue scope h with
        | Ok value -> lookupSymbolsInArgList scope (value :: lst) t
        | Error err -> Error err

let rec private evalList scope evaluatedList = function
    | [] -> Ok (List.rev evaluatedList)
    | h::t ->
        match (evaluate scope h) with
        | Ok value -> evalList scope (value :: evaluatedList) t
        | Error err -> Error err
and evaluate (scope: Scope) (value: Value) : Result<Value, EvalError> =
    match value.Type with
    | Expression [] -> Ok Parser.unitValue
    | Expression (funcIdent::args) ->
        evaluate scope funcIdent
        >>= expectSymbol
        >>= fun identifier ->
            // lookup function by identifier
            // then use bindArgs to evaluate and bind arguments to names in sequence if not builtin

            if identifier = "+" then
                expectArgsLength 2 args 
                >>! fun (len, expected) ->
                        { ErrorTypes.EvalError.Type = WrongNumberOfSuppliedArguments (identifier, len, expected) 
                          Position = funcIdent.Position}
                // evaluate all arguments
                >>= evalList scope []
                // substitute any symbols with the values they represent
                >>= lookupSymbolsInArgList scope []
                <!> fun args ->
                        (args, createScope (Some scope))
                //>>= bindArgs // bind args to new eval scope. also needs to do type-checking if applicable...
                >>= fun (args, _) ->
                        (unwrapAtomNum args.[0]) + (unwrapAtomNum args.[1])
                        |> NumberAtom
                        |> Value.createAnon
                        |> Ok
            else
                Error { ErrorTypes.EvalError.Type = UnboundIdentifier identifier 
                        Position = funcIdent.Position }

            //identifier |> StringAtom |> Value.createAnon |> Ok
    | _ -> Ok value
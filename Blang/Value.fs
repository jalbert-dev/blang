module Blang.Value

open RuntimeTypes

let rec private stringifyExpr expr =
    seq {
        yield "("
        yield seq {
            for x in expr -> stringify x
        } |> String.concat " "
        yield ")"
    } |> String.concat ""
and stringify { Value.Type = valueType } =
    match valueType with
    | NumberAtom x -> sprintf "%g" x
    | SymbolAtom x -> sprintf "%s" x
    | StringAtom x -> sprintf "\"%s\"" x
    | Expression x -> stringifyExpr x

let createWithPosition position valueType =
    { Value.Type = valueType;
      Position = Some position }
let createAnon valueType = 
    { Value.Type = valueType;
      Position = None }

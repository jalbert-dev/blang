namespace Blang

module ValueTypes =
    type Sexpr =
        | Num of double
        | Symbol of string
        | String of string
        | Expr of Sexpr list
        | Quote of Sexpr list

open ValueTypes

module Sexpr =
    let rec private stringifyExpr expr =
        seq {
            yield "("
            yield seq {
                for x in expr -> stringify x
            } |> String.concat " "
            yield ")"
        } |> String.concat ""
    and stringify = function
        | Num x -> sprintf "%g" x
        | Symbol x -> sprintf "%s" x
        | String x -> sprintf "\"%s\"" x
        | Expr x -> stringifyExpr x
        | Quote x -> "'" + stringifyExpr x

    let private isNum c =
        c >= '0' && c <= '9'
       
    let private isDecimal c =
        c = '.'

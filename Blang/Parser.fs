module Blang.Parser

open Blang.ParserTypes
open Blang.RuntimeTypes

/// For now, unit value is just empty expression "()"... so it's not really a unit type but whatever
let unitValue = Expression []

type TokenProducer<'State> = 'State -> Result<Token * 'State, EvalError>

let private createError errType position =
    { EvalError.Type = errType; Position = position }

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a

let rec private parseExpr (producerState, produceToken: 'a -> Result<Token * 'a, EvalError>) exprBodyAcc depth =
    producerState |> produceToken
    >>= fun (token, state) ->
            match token.Type with
            | EOF -> Error (createError ExpectedValue token.Position)
            | RParen -> Ok (List.rev exprBodyAcc |> Expression, state)
            | _ -> 
                parseVal token (state, produceToken) depth 
                >>= fun (value, state) -> 
                        parseExpr (state, produceToken) (value :: exprBodyAcc) depth
and private parseVal token (producerState, produceToken) depth =
    match token.Type with
    | EOF -> (unitValue, producerState) |> Ok
    | Number x -> (NumberAtom x, producerState) |> Ok
    | String x -> (StringAtom x, producerState) |> Ok
    | Symbol x -> (SymbolAtom x, producerState) |> Ok
    | LParen -> parseExpr (producerState, produceToken) [] (depth + 1)
    | _ -> Error (createError (UnexpectedToken token.Type) token.Position)

let parse (tokenizerStartState: 'a, tokenProducer: TokenProducer<'a>) : Result<Value * 'a, EvalError> = 
    tokenProducer tokenizerStartState
    >>= fun (firstToken, tokenizerState) ->
            parseVal firstToken (tokenizerState, tokenProducer) 0

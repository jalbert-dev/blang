module Blang.Parser

open Blang.ParserTypes
open Blang.RuntimeTypes
open Blang.ErrorTypes

/// For now, unit value is just empty expression "()"... so it's not really a unit type but whatever
let unitValue = [] |> (Expression >> Value.createAnon)

type TokenProducer<'State> = 'State -> Result<Token * 'State, EvalError>

let private createError errType position =
    { EvalError.Type = errType; Position = position }

let private ( >>= ) a b = Result.bind b a
let private ( <!> ) a b = Result.map b a

let (|AtomicValue|_|) = function
    | Number x -> Some (NumberAtom x)
    | String x -> Some (StringAtom x)
    | Symbol x -> Some (SymbolAtom x)
    | _ -> None

let rec private parseExpr (producerState, produceToken: 'a -> Result<Token * 'a, EvalError>) position exprBodyAcc =
    producerState |> produceToken
    >>= fun (token, state) ->
            match token.Type with
            | EOF -> Error (createError ExpectedValue (Some token.Position))
            | RParen -> Ok (List.rev exprBodyAcc |> Expression |> Value.createWithPosition position, state)
            | _ -> 
                parseVal token (state, produceToken)
                >>= fun (value, state) -> 
                        parseExpr (state, produceToken) position (value :: exprBodyAcc)
and private parseVal token (producerState, produceToken) =
    match token.Type with
    | EOF -> (unitValue, producerState) |> Ok
    | AtomicValue x -> (x |> Value.createWithPosition token.Position, producerState) |> Ok
    | LParen -> parseExpr (producerState, produceToken) token.Position []
    | _ -> Error (createError (UnexpectedToken token.Type) (Some token.Position))

let parse (tokenizerStartState: 'a, tokenProducer: TokenProducer<'a>) : Result<Value * 'a, EvalError> = 
    tokenProducer tokenizerStartState
    >>= fun (firstToken, tokenizerState) ->
            parseVal firstToken (tokenizerState, tokenProducer)

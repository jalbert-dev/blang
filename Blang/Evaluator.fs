module Blang.Evaluator

open Blang.ParserTypes
open Blang.RuntimeTypes

// Given a LexState and a global Scope, the Evaluator evaluates a single expression
// at a time and returns the resulting Value and an updated Scope.

/// For now, unit value is just empty expression "()"... so it's not really a unit type but whatever
let private unitValue = Expression []

type private ParseState = { Depth: int }

let private createError errType position =
    { EvalError.Type = errType; Position = position }

let private ( >>= ) a b = Result.bind b a

// TODO: no need to pass scope here because it never evaluates anything, but evalStepRootState requires it

// this differs from the root parser state in that this state accumulates values
// and returns them as an expression but can't perform evaluation.
let rec private parseExpression src scope state acc =
    match Lexer.next src with
    | Ok (token, rest) ->
        match token.Type with
        // when we hit RParen, return contents list as Expression
        | RParen -> 
            Ok (Expression acc, rest)
        // if next token isn't RParen, evalStepRootState in non-eval context
        // and stuff resulting value into the list of expression contents
        | _ ->
            evalStepRootState src scope false state 
            >>= fun (value, rest, newScope) -> parseExpression rest newScope state (value :: acc)
    | Error err ->
        Error (err, scope)

and private evalStepRootState src scope shouldEval state =
    match Lexer.next src with
    | Ok (token, rest) ->
        match token.Type with
        | Number x -> Ok (NumberAtom x, rest, scope)
        | String x -> Ok (StringAtom x, rest, scope)
        | Symbol x -> Ok (SymbolAtom x, rest, scope)

        | EOF -> 
            // parsing EOF is fine as long as we're not in the middle of an expression
            if state.Depth = 0 then
                Ok (unitValue, rest, scope)
            else
                Error (createError UnexpectedEOF src.Position, scope)

        | SingleQuote ->
            // expect and receive a valid expression, then return it immediately
            let nextResult = evalStepRootState rest scope false state
            match nextResult with
            | Ok (Expression _, _, _) -> nextResult
            | Error _ -> nextResult
            | _ -> Error (createError ExpectedExpression rest.Position, scope)

        | LParen ->
            // expect and receive a valid expression. if we're in eval context, eval it
            // in context of scope, and return the evaluated value and the new scope.
            // else just return it as if it were quoted
            parseExpression rest scope state []
            >>= fun (expr, rest) ->
                if shouldEval then
                    failwith "Can't evaluate expressions yet, sorry boss"
                else
                    // no evaluation means no modification of scope
                    Ok (expr, rest, scope)

        | RParen ->
            Error (createError UnexpectedCloseExpression token.Position, scope)

    | Error err ->
        Error (err, scope)

let rec evalStep (source: LexState) (evalScope: Scope) : Result<Value * LexState * Scope, EvalError * Scope> =
    evalStepRootState source evalScope true { Depth = 0 }

let createScope (parent: Scope option) =
    { EnclosingScope = parent
      SymbolTable = Map.empty }

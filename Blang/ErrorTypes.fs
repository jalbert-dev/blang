module Blang.ErrorTypes

open Blang.ParserTypes

type EvalErrorType =
    // syntax errors
    | UnterminatedString
    | UnexpectedCharacter of char
    | InvalidNumber

    // parse errors
    | UnexpectedToken of TokenType
    | ExpectedValue

    // eval errors
    | FunctionIdentifierMustBeSymbol of RuntimeTypes.ValueType
    | ExpectedNumber of RuntimeTypes.ValueType
    | ExpectedSymbol of RuntimeTypes.ValueType
    | ExpectedString of RuntimeTypes.ValueType
    | ExpectedExpression of RuntimeTypes.ValueType
    | UnboundIdentifier of string
    | WrongNumberOfSuppliedArguments of int * int
    | ErrorEvaluatingFunction of string * EvalError
and EvalError =
    { Type: EvalErrorType
      Position: RuntimeTypes.LineInfo option }
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
    | ExpectedNumber of RuntimeTypes.ValueType
    | ExpectedSymbol of RuntimeTypes.ValueType
    | ExpectedString of RuntimeTypes.ValueType
    | ExpectedExpression of RuntimeTypes.ValueType
    | UnboundIdentifier of string
    | WrongNumberOfSuppliedArguments of int * int
    | ErrorEvaluatingFunction of string * EvalError
    | InvalidFunctionDefinition of RuntimeTypes.ValueType
    | InvalidNativeFunctionName of string
    | CannotTakeHeadOfEmptyList
    | CannotTakeTailOfEmptyList

    | UserThrownError of RuntimeTypes.ValueType
and EvalError =
    { Type: EvalErrorType
      Position: RuntimeTypes.LineInfo option }
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
    | UnboundIdentifier of string
    | WrongNumberOfSuppliedArguments of string * int * int

type EvalError =
    { Type: EvalErrorType
      Position: RuntimeTypes.LineInfo option }
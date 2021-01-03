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

type EvalError =
    { Type: EvalErrorType
      Position: LineInfo }
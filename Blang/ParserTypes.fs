module Blang.ParserTypes

type LineInfo =
    { Line: int
      Character: int }

type TokenType =
    | Number of double
    | String of string
    | Symbol of string
    | LParen
    | RParen
    | SingleQuote
    | EOF

type Token =
    { Type: TokenType
      Position: LineInfo }

type LexState =
    { Source: string
      Index: int 
      Position: LineInfo }

type EvalErrorType =
    // syntax errors
    | UnterminatedString
    | UnexpectedCharacter of char
    | InvalidNumber

    // parse errors
    | UnexpectedEOF
    | UnexpectedCloseExpression
    | ExpectedExpression

type EvalError =
    { Type: EvalErrorType
      Position: LineInfo }

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

type LexErrorType =
    | UnterminatedString
    | UnexpectedCharacter of char
    | InvalidNumber

type LexError =
    { Type: LexErrorType
      Position: LineInfo }

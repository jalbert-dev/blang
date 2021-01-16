module Blang.ParserTypes

type TokenType =
    | Number of double
    | String of string
    | Symbol of string
    | QuotedSymbol of string
    | LParen
    | QuotedLParen
    | RParen
    | EOF

type Token =
    { Type: TokenType
      Position: RuntimeTypes.LineInfo }

type LexState =
    { Source: string
      Index: int 
      Position: RuntimeTypes.LineInfo }

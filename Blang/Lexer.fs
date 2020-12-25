namespace Blang

type LineInfo =
    { Line: int
      Character: int }

type LexState =
    { Source: string
      Index: int 
      Position: LineInfo }

module Lexer =
    type LexErrorType =
        | UnterminatedString
        | UnexpectedCharacter of char
    type LexError =
        { Type: LexErrorType
          Position: LineInfo }

    type TokenType =
        | Number of int
        | String of string
        | Symbol of string
        | LParen
        | RParen
        | SingleQuote
        | EOF
    type Token =
        { Type: TokenType
          Position: LineInfo }

    let private forbiddenSymbolChars = [ '\''; '('; ')'; '"'; ]

    let private isNewline c =
        c = '\n'
    let private isWhitespace c =
        isNewline c || List.contains c [' '; '\r'; '\t']
    let private isNumeric c =
        c >= '0' && c <= '9'
    let private isStringDelimiter c =
        c = '"'
    let private isValidSymbolChar c =
        not (List.contains c forbiddenSymbolChars) &&
        not (isWhitespace c)
    let private isValidSymbolStarter c =
        isValidSymbolChar c && not (isNumeric c)

    // Lexer functions
    let private atEof lexer = lexer.Index >= String.length lexer.Source
    let private currentChar lexer = lexer.Source.[lexer.Index]
    let private moveNext lexer =
        if atEof lexer then 
            lexer 
        else
            { lexer with
                Index = lexer.Index + 1
                Position = 
                    match currentChar lexer with
                    | c when isNewline c -> 
                        { Line = lexer.Position.Line + 1; Character = 1 }
                    | _ ->
                        { lexer.Position with Character = lexer.Position.Character + 1 } }
    let private createToken (pos: LineInfo) (tokType: TokenType) =
        { Token.Type = tokType; Position = pos }
    let private createError (lexer: LexState) (errType: LexErrorType) =
        ({ LexError.Type = errType; Position = lexer.Position })

    // Lexer active patterns
    let inline private ifTrueThen value f = if f then Some value else None
    let private (|LexerFinished|_|) lexer =
        (atEof lexer) |> ifTrueThen lexer
    let private (|MatchBy|_|) f lexer =
        (lexer |> currentChar |> f) |> ifTrueThen (moveNext lexer)
    let private (|MatchChar|_|) c lexer =
        (currentChar lexer = c) |> ifTrueThen (moveNext lexer)
    let private (|MatchAny|) = moveNext
    let private (|CaptureAny|) lexer = (moveNext lexer, currentChar lexer)

    // Lex states and helpers
    let rec private eatLine = function
        | LexerFinished lexer -> lexer
        | MatchBy(isNewline) rest -> rest
        | MatchAny rest -> rest |> eatLine

    let private lexString lex =
        let startIdx = lex.Index
        let rec loop = function
            | LexerFinished _ -> Error (createError lex UnterminatedString)
            | MatchBy(isStringDelimiter) rest -> Ok (String rest.Source.[startIdx + 1..rest.Index - 2], rest)
            | MatchAny rest -> loop rest
        lex |> moveNext |> loop

    let create (sourceString: string) =
        { Source = sourceString
          Index = 0
          Position = 
              { Line = 1
                Character = 1 } }

    let rec next (lexer: LexState) : Result<Token * LexState, LexError> =
        let emit tok pos next = (createToken pos tok, next) |> Ok
        let emitFromTuple pos (tok, next) = emit tok pos next
        let ( >>= ) a b = Result.bind b a

        match lexer with
        | LexerFinished _ -> lexer |> emit EOF lexer.Position
        | MatchChar('(') rest -> rest |> emit LParen lexer.Position
        | MatchChar(')') rest -> rest |> emit RParen lexer.Position
        | MatchChar('\'') rest -> rest |> emit SingleQuote lexer.Position
        | MatchChar('#') rest -> rest |> eatLine |> next
        | MatchBy(isWhitespace) rest -> rest |> next
        | MatchBy(isStringDelimiter) _ -> lexer |> lexString >>= emitFromTuple lexer.Position
        //| c when isNumeric c -> str |> lexNumeric
        //| c when isValidSymbolStarter c -> str |> lexSymbol
        | CaptureAny (_, c) -> c |> UnexpectedCharacter |> createError lexer |> Error
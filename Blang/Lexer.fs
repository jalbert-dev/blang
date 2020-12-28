module Blang.Lexer

open ParserTypes

// Lexer categorizers
let private forbiddenSymbolChars = 
    [ '\''; '('; ')'; '"'; '#' ]
let private isNewline c =
    c = '\n'
let private isWhitespace c =
    isNewline c || List.contains c [' '; '\r'; '\t']
let private isNumeric c =
    c >= '0' && c <= '9'
let private isNumericStarter c =
    isNumeric c || c = '-'
let private isDecimalPoint c =
    c = '.'
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
let private createError (lexer: LexState) (errType: EvalErrorType) =
    ({ EvalError.Type = errType; Position = lexer.Position })

// Lexer active patterns
let inline private ifTrueThen value f = if f then Some value else None
let private (|LexerFinished|_|) lexer =
    (atEof lexer) |> ifTrueThen lexer
let private (|PeekBy|_|) f lexer =
    (lexer |> currentChar |> f) |> ifTrueThen lexer
let private (|PeekChar|_|) c lexer =
    (currentChar lexer = c) |> ifTrueThen lexer
let private (|MatchBy|_|) f lexer =
    (lexer |> currentChar |> f) |> ifTrueThen (moveNext lexer)
let private (|MatchChar|_|) c lexer =
    (currentChar lexer = c) |> ifTrueThen (moveNext lexer)
let private (|MatchAny|) = moveNext
let private (|CaptureAny|) lexer = (moveNext lexer, currentChar lexer)

// Lex states and helpers
let create (sourceString: string) =
    { Source = sourceString
      Index = 0
      Position = 
          { Line = 1
            Character = 1 } }

let rec private eatUntilNewline = function
    | LexerFinished rest -> rest
    | PeekBy(isNewline) rest -> rest
    | MatchAny rest -> rest |> eatUntilNewline

let private lexString lex =
    let rec loop = function
        | LexerFinished _ -> Error (createError lex UnterminatedString)
        | MatchBy(isStringDelimiter) rest -> Ok (String rest.Source.[lex.Index + 1..rest.Index - 2], rest)
        | MatchAny rest -> loop rest
    lex |> moveNext |> loop

let private lexSymbol lex =
    let isInvalidSymbolChar = isValidSymbolChar >> not
    let rec loop = function
        | LexerFinished rest -> Ok (Symbol rest.Source.[lex.Index..rest.Index - 1], rest)
        | PeekBy(isInvalidSymbolChar) rest -> Ok (Symbol rest.Source.[lex.Index..rest.Index - 1], rest)
        | MatchAny rest -> loop rest
    lex |> loop

let private lexNumeric negative lex =
    let accept endState canAccept =
        if canAccept then
            Ok (lex.Source.[lex.Index..endState.Index - 1] 
                |> double 
                |> (fun n -> if negative then -n else n)
                |> Number, endState)
        else
            Error (createError lex InvalidNumber)

    let rec loop isAfterDecimal hasDigit = function
        | LexerFinished rest -> accept rest hasDigit
        | PeekBy(isDecimalPoint) rest -> 
            if isAfterDecimal then
                Error (createError rest (rest |> currentChar |> UnexpectedCharacter))
            else
                loop true hasDigit (rest |> moveNext)
        | MatchBy(isNumeric) rest -> loop isAfterDecimal true rest
        | rest -> accept rest hasDigit
    lex |> loop false false

let private lexAmbiguousMinus atMinusState =
    match atMinusState |> moveNext with
    // "-[0-9]" = number
    | PeekBy(isNumeric) afterMinusState -> lexNumeric true afterMinusState
    // "-." = start of number
    | PeekBy(isDecimalPoint) afterMinusState -> lexNumeric true afterMinusState
    // "-[_]" = start of symbol
    | _ -> lexSymbol  atMinusState

let rec next (lexer: LexState) : Result<Token * LexState, EvalError> =
    let emit tok pos next = (createToken pos tok, next) |> Ok
    let emitFromTuple pos (tok, next) = emit tok pos next
    let ( >>= ) a b = Result.bind b a

    match lexer with
    | LexerFinished _ -> lexer |> emit EOF lexer.Position
    // reserved characters
    | MatchChar('(') rest -> rest |> emit LParen lexer.Position
    | MatchChar(')') rest -> rest |> emit RParen lexer.Position
    | MatchChar('\'') rest -> rest |> emit SingleQuote lexer.Position
    // comments/whitespace
    | MatchChar('#') rest -> rest |> eatUntilNewline |> next
    | MatchBy(isWhitespace) rest -> rest |> next
    // strings
    | PeekBy(isStringDelimiter) rest -> rest |> lexString >>= emitFromTuple lexer.Position
    // numbers
    | PeekChar('-') rest -> rest |> lexAmbiguousMinus >>= emitFromTuple lexer.Position
    | PeekBy(isDecimalPoint) rest -> rest |> lexNumeric false >>= emitFromTuple lexer.Position
    | PeekBy(isNumeric) rest -> rest |> lexNumeric false >>= emitFromTuple lexer.Position
    // symbols
    | PeekBy(isValidSymbolStarter) rest -> rest |> lexSymbol >>= emitFromTuple lexer.Position
    // unexpected input
    | CaptureAny (_, c) -> c |> UnexpectedCharacter |> createError lexer |> Error

namespace Lexer

open Microsoft.FSharp.Core
open System

type TokenType =
    | EOF = -1
    | NEWLINE = 0
    | NUMBER = 1
    | IDENT = 2
    | STRING = 3
    | LABEL = 101
    | GOTO = 102
    | PRINT = 103
    | INPUT = 104
    | LET = 105
    | IF = 106
    | THEN = 107
    | ENDIF = 108
    | WHILE = 109
    | REPEAT = 110
    | ENDWHILE = 111
    | EQ = 201
    | PLUS = 202
    | MINUS = 203
    | ASTERISK = 204
    | SLASH = 205
    | EQEQ = 300
    | NOTEQ = 301
    | LT = 208
    | LTEQ = 302
    | GT = 210
    | GTEQ = 304

type Token = { Text: string; Type: TokenType }

module Lexer =
    let abort message =
        sprintf "Lexing Error. %s" message |> failwith

    let tokenFromString tokenText tokenType = { Text = tokenText; Type = tokenType }
    let tokenFromChar (c: char) t = { Text = string c; Type = t }

    let nextChar (tokenArray: char []) = tokenArray.[0]

    let rec skipWhiteSpace (tokenArray: char []) =
        match tokenArray.[0] with
        | ' ' -> skipWhiteSpace tokenArray.[1..]
        | '\t' -> skipWhiteSpace tokenArray.[1..]
        | '\r' -> skipWhiteSpace tokenArray.[1..]
        | _ -> tokenArray

    let rec skipComment (tokenArray: char []) =
        match tokenArray.[0] with
        | '\n' -> tokenArray
        | _ -> skipComment tokenArray.[1..]

    let skipOffset (token: Token) =
        match token.Type with
        | TokenType.GTEQ
        | TokenType.LTEQ
        | TokenType.NOTEQ -> token.Text.Length
        | TokenType.STRING -> token.Text.Length + 2
        | TokenType.NUMBER -> token.Text.Length + 1
        | _ ->
            let tokenTypeRaw = (int) token.Type
            match tokenTypeRaw > 100 || tokenTypeRaw < 200 with
            | true -> token.Text.Length
            | false -> 1

    let buildStringToken (tokenArray: char []) =
        let endIndex = tokenArray |> Array.findIndex ((=) '\"')
        let stringText = string tokenArray.[..endIndex - 1]

        let invalidChars = [| '\r'; '\t'; '\\'; '%' |]

        let stringInvalid =
            invalidChars
            |> Array.map stringText.Contains
            |> Array.sumBy (function
                | true -> 1
                | false -> 0) > 0

        match stringInvalid with
        | true -> "Illegal character in string." |> abort
        | false -> tokenFromString stringText TokenType.STRING

    let peek (tokenArray: char []) =
        match tokenArray.Length = 1 with
        | true -> '\u0004'
        | false -> tokenArray.[1]

    let handleToken (tokenArray: char []) =
        let currChar = nextChar tokenArray
        let nextChar = peek tokenArray

        match (currChar, nextChar) with
        | ('+', _) -> tokenFromChar currChar TokenType.PLUS
        | ('-', _) -> tokenFromChar currChar TokenType.MINUS
        | ('*', _) -> tokenFromChar currChar TokenType.ASTERISK
        | ('/', _) -> tokenFromChar currChar TokenType.SLASH
        | ('\n', _) -> tokenFromChar currChar TokenType.NEWLINE
        | ('\u0004', _) -> tokenFromChar currChar TokenType.EOF
        | ('=', '=') -> tokenFromString (string currChar + string nextChar) TokenType.EQ
        | ('=', _) -> tokenFromChar currChar TokenType.EQ
        | ('>', '=') -> tokenFromString (string currChar + string nextChar) TokenType.GTEQ
        | ('>', _) -> tokenFromChar currChar TokenType.GT
        | ('<', '=') -> tokenFromString (string currChar + string nextChar) TokenType.LTEQ
        | ('<', _) -> tokenFromChar currChar TokenType.LT
        | ('!', '=') -> tokenFromString (string currChar + string nextChar) TokenType.NOTEQ
        | ('!', _) -> "Expected !=, got !" |> abort
        | ('\"', _) -> buildStringToken tokenArray.[1..]
        | _ ->
            sprintf "%s%d" "Unknown Token: " (int currChar - int 0)
            |> abort

    let rec getValue (chars: char []) newValue matchFunc =
        let currentChar = chars.[0]
        match matchFunc currentChar with
        | false -> newValue
        | true -> getValue chars.[1..] (newValue + (string currentChar)) matchFunc

    let handleNumberToken tokenArray =
        let numberMatch c = Char.IsDigit c || c = '.'
        let numberString = getValue tokenArray "" numberMatch
        printf "%s\n" numberString
        tokenFromString numberString TokenType.NUMBER

    let handleAlphaToken tokenArray =
        let alphaString = getValue tokenArray "" Char.IsLetter
        printf "%s\n" alphaString

        let tokenType =
            match alphaString with
            | "LABEL" -> TokenType.LABEL
            | "GOTO" -> TokenType.GOTO
            | "PRINT" -> TokenType.PRINT
            | "INPUT" -> TokenType.INPUT
            | "LET" -> TokenType.LET
            | "IF" -> TokenType.IF
            | "THEN" -> TokenType.THEN
            | "ENDIF" -> TokenType.ENDIF
            | "WHILE" -> TokenType.WHILE
            | "REPEAT" -> TokenType.REPEAT
            | "ENDWHILE" -> TokenType.ENDWHILE
            | _ -> TokenType.IDENT

        tokenFromString alphaString tokenType

    let getToken (tokenArray: char []) =
        let currChar = tokenArray.[0]

        let alphaNumTuple =
            (Char.IsLetter currChar, Char.IsDigit currChar)

        let handleFunc =
            match alphaNumTuple with
            | (false, false) -> handleToken
            | (true, _) -> handleAlphaToken
            | (_, true) -> handleNumberToken

        handleFunc tokenArray

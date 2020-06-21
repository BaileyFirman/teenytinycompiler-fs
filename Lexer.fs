namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module LexerFuncs =
    let private abort message =
        sprintf "Lexing Error. %s" message |> failwith

    let tokenFromString tokenText tokenType = { Text = tokenText; Type = tokenType }
    let tokenFromChar (c: char) t = { Text = string c; Type = t }

    let nextChar (chars: char []) = chars.[0]

    let peek (chars: char []) =
        match chars.Length = 1 with
        | true -> '\u0004'
        | false -> chars.[1]

    let rec skipWhiteSpace (chars: char []) =
        match chars.[0] with
        | ' '
        | '\t'
        | '\r' -> skipWhiteSpace chars.[1..]
        | _ -> chars

    let rec skipComment (chars: char []) =
        match chars.[0] with
        | '\n' -> chars
        | _ -> skipComment chars.[1..]

    let skipOffset token =
        let identifierOrSingle tokenType =
            match tokenType > 100 || tokenType < 200 with
            | true -> token.Text.Length
            | false -> 1

        match token.Type with
        | TokenType.GTEQ
        | TokenType.LTEQ
        | TokenType.NOTEQ -> token.Text.Length
        | TokenType.STRING -> token.Text.Length + 2 // " & "
        | TokenType.NUMBER -> token.Text.Length + 1
        | _ -> identifierOrSingle (int token.Type)

    let private buildStringToken chars =
        let endIndex = chars |> Array.findIndex ((=) '\"')
        let stringText = string chars.[..endIndex - 1]
        let invalidStringChars = [| '\r'; '\t'; '\\'; '%' |]

        let stringValid =
            invalidStringChars
            |> Array.map stringText.Contains
            |> Array.sumBy (function
                | true -> 1
                | false -> 0) = 0

        match stringValid with
        | true -> tokenFromString stringText TokenType.STRING
        | false -> "Illegal character in string." |> abort

    let private handleToken chars =
        let currChar = nextChar chars
        let nextChar = peek chars

        let abortOnUnknown currChar =
            sprintf "%s%d" "Unknown Token: " (int currChar)
            |> abort

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
        | ('\"', _) -> buildStringToken chars.[1..]
        | _ -> abortOnUnknown currChar

    let rec private buildValueFromChars chars newValue matchFunc =
        let currentChar = nextChar chars
        let charMatch = matchFunc currentChar

        match charMatch with
        | true -> buildValueFromChars chars.[1..] (newValue + (string currentChar)) matchFunc
        | false -> newValue

    let private handleNumberToken chars =
        let numberString =
            buildValueFromChars chars "" (fun c -> Char.IsDigit c || c = '.')

        tokenFromString numberString TokenType.NUMBER

    let private handleAlphaToken chars =
        let alphaString =
            buildValueFromChars chars "" Char.IsLetter

        printf "\n >>> %s <<<\n" alphaString

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

    let getToken chars =
        let currChar = nextChar chars

        let alphaNumTuple =
            (Char.IsLetter currChar, Char.IsDigit currChar)

        let handleFunc =
            match alphaNumTuple with
            | (false, false) -> handleToken
            | (true, _) -> handleAlphaToken
            | (_, true) -> handleNumberToken

        handleFunc chars

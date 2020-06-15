namespace Lexer

open Microsoft.FSharp.Core

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
    | EQEQ = 206
    | NOTEQ = 207
    | LT = 208
    | LTEQ = 209
    | GT = 210
    | GTEQ = 211

type Token = { Text: string; Type: TokenType }

module Lexer =
    let abort message =
        let failMessage = sprintf "%s%s" "Lexing error. " message
        failwith failMessage

    let private tokenFromChar (c: char, t: TokenType) = { Text = c.ToString(); Type = t }

    let private tokenFromString (s: string, t: TokenType) = { Text = s; Type = t }

    let nextChar (tokenArray: char []): char = tokenArray.[0]

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
        | TokenType.GTEQ -> 2
        | TokenType.LTEQ -> 2
        | TokenType.NOTEQ -> 2
        | TokenType.STRING -> token.Text.Length + 2
        | _ -> 1

    let findIndex arr elem = arr |> Array.findIndex ((=) elem) 

    let validateString (string: string) =
        let containsReturn = string.Contains('\r')
        let containsTab = string.Contains('\t')
        let containsBackslash = string.Contains('\\')
        let containsFraction = string.Contains('%')
        containsReturn || containsTab || containsBackslash || containsFraction

    let extractString(tokenArray: char[]): Token =
        let endIndex = findIndex tokenArray '\"'
        let stringText = System.String tokenArray.[..endIndex - 1]
        printf "%s\n" stringText
        match validateString stringText with
        | true -> "Illegal character in string." |> failwith
        | false -> { Text = stringText; Type = TokenType.STRING }
        
    let peek (tokenArray: char []): char =
        match tokenArray.Length = 1 with
        | true -> '0'
        | false -> tokenArray.[1]

    let getToken (currChar: char, tokenArray: char []) =
        match currChar with
        | '+' -> tokenFromChar (currChar, TokenType.PLUS)
        | '-' -> tokenFromChar (currChar, TokenType.MINUS)
        | '*' -> tokenFromChar (currChar, TokenType.ASTERISK)
        | '/' -> tokenFromChar (currChar, TokenType.SLASH)
        | '\n' -> tokenFromChar (currChar, TokenType.NEWLINE)
        | '\t' -> tokenFromChar (currChar, TokenType.NEWLINE)
        | '0' -> tokenFromChar (currChar, TokenType.EOF)
        | '=' -> tokenFromChar (currChar, TokenType.EQ)
        | '>' ->
            let nextChar = peek tokenArray
            match nextChar with
            | '=' -> tokenFromString (System.String [| currChar; nextChar |], TokenType.GTEQ)
            | _ -> tokenFromChar (currChar, TokenType.GT)
        | '<' ->
            let nextChar = peek tokenArray
            match nextChar with
            | '=' -> tokenFromString (System.String [| currChar; nextChar |], TokenType.LTEQ)
            | _ -> tokenFromChar (currChar, TokenType.LT)
        | '!' ->
            let nextChar = peek tokenArray
            match nextChar with
            | '=' -> tokenFromString (System.String [| currChar; nextChar |], TokenType.NOTEQ)
            | _ -> "Expected !=, got !" |> abort
        | '\"' -> extractString tokenArray.[1..]
        | _ -> sprintf "%s%c%d" "Unknown Token: " currChar (int currChar - int 0) |> abort

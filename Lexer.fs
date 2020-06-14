namespace Lexer

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

type Token = { Text: char; Type: TokenType }

module Lexer =
    let abort message =
        let failMessage = sprintf "%s%s" "Lexing error. " message
        failwith failMessage

    let nextChar (tokenArray: char []): char = tokenArray.[0]

    let skipWhiteSpace (currChar: char) =
        match currChar with
        | ' ' -> 2
        | '\t' -> 2
        | '\r' -> 2
        | _ -> 1

    let peek (tokenArray: char []): char =
        match tokenArray.Length = 1 with
        | true -> '0'
        | false -> tokenArray.[1]

    let getToken (currChar: char) =
        match currChar with
        | '+' ->
            { Text = currChar
              Type = TokenType.PLUS }
        | '-' ->
            { Text = currChar
              Type = TokenType.MINUS }
        | '*' ->
            { Text = currChar
              Type = TokenType.ASTERISK }
        | '/' ->
            { Text = currChar
              Type = TokenType.SLASH }
        | '\n' ->
            { Text = currChar
              Type = TokenType.NEWLINE }
        | '0' ->
            { Text = currChar
              Type = TokenType.EOF }
        | _ -> sprintf "%s%c" "Unknown Token: " currChar |> abort

namespace Types

module Tokens =
    type TokenType =
        | EOF = -1
        | NEWLINE = 0
        | NUMBER = 1
        | IDENT = 2
        | STRING = 3
        | WHITESPACE = 4
        | COMMENT = 5
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

    let matchIdentifier string =
        match string with
        | "ENDIF" -> TokenType.ENDIF
        | "ENDWHILE" -> TokenType.ENDWHILE
        | "GOTO" -> TokenType.GOTO
        | "IF" -> TokenType.IF
        | "INPUT" -> TokenType.INPUT
        | "LABEL" -> TokenType.LABEL
        | "LET" -> TokenType.LET
        | "PRINT" -> TokenType.PRINT
        | "REPEAT" -> TokenType.REPEAT
        | "THEN" -> TokenType.THEN
        | "WHILE" -> TokenType.WHILE
        | _ -> TokenType.IDENT

    type Token = { Text: string; Type: TokenType }

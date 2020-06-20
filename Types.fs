namespace Types

module Tokens =
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

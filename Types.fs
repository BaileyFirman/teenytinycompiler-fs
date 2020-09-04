namespace TeenyTiny

module Types =
    type TokenType =
        | EOF
        | NEWLINE
        | NUMBER
        | IDENT
        | STRING
        | WHITESPACE
        | COMMENT
        | LABEL
        | GOTO
        | PRINT
        | INPUT
        | LET
        | IF
        | THEN
        | ENDIF
        | WHILE
        | REPEAT
        | ENDWHILE
        | EQ
        | PLUS
        | MINUS
        | ASTERISK
        | SLASH
        | EQEQ
        | NOTEQ
        | LT
        | LTEQ
        | GT
        | GTEQ

    let stringToKeywordType string =
        match string with
        | "ENDIF" -> ENDIF
        | "ENDWHILE" -> ENDWHILE
        | "GOTO" -> GOTO
        | "IF" -> IF
        | "INPUT" -> INPUT
        | "LABEL" -> LABEL
        | "LET" -> LET
        | "PRINT" -> PRINT
        | "REPEAT" -> REPEAT
        | "THEN" -> THEN
        | "WHILE" -> WHILE
        | _ -> IDENT

    let isTypeComparisonOperator tokenType =
        match tokenType with
        | GT
        | GTEQ
        | LT
        | LTEQ
        | EQEQ
        | NOTEQ -> true
        | _ -> false

    type Token = { Text: string; Type: TokenType }
    let eofToken = { Type = EOF; Text = "EOF" }

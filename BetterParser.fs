namespace Parser

open Types.Tokens

module BetterParser =
    let private tokenKind token = token.Type
    let private tokenType token = token.Type
    let private next pointer = pointer + 1
    let private dummyToken = { Text = ""; Type = TokenType.EOF }

    let parseTokenStream (tokenStream: Token []) =
        let getToken pointer = tokenStream.[pointer]

        let rec newline pointer =
            let currentKind = pointer |> getToken |> tokenKind
            let nextPointer = next pointer
            match currentKind with
            | TokenType.NEWLINE ->
                printf "PARSE: NEWLINE\n"
                newline nextPointer
            | _ -> pointer

        let handlePrint pointer =
            // Previous Token must have been a print
            let currentToken = getToken pointer

            let newPointer = 
                match currentToken.Type with
                | TokenType.STRING ->
                    printf "PARSE: STATEMENT-PRINT\n"
                    next pointer
                | _ ->
                    failwith "NOT IMPLEMENTED"
                    // probably an expression

            newline newPointer

        let handleComparison pointer =
            next pointer

        let handleStatement pointer =
            next pointer

        let handleIf pointer =
            printf "PARSE: STATEMENT-IF"
            let currentToken = getToken pointer
            let nextPointer = next pointer
            let comparisonPointer = handleComparison nextPointer

            let endOfPointer = newline comparisonPointer

            let rec statementCheck pointer =
                match pointer |> getToken |> tokenType with
                | TokenType.ENDIF -> next pointer
                | _ -> handleStatement pointer
            
            statementCheck endOfPointer


        let rec parseLoop (stream: unit) streamPointer =
            let currentToken = getToken streamPointer

            let currentTokenType = tokenKind currentToken
            // printf "PARSE: %s\n"
            // <| currentTokenType.ToString()

            let nextToken =
                match tokenKind currentToken with
                | TokenType.EOF -> dummyToken
                | _ -> streamPointer |> next |> getToken

            let action () =
                match currentTokenType with
                | TokenType.PRINT -> handlePrint (next streamPointer)
                | TokenType.IF -> handleIf (next streamPointer)
                | _ -> next streamPointer

            let newPointer = action ()

            match tokenKind currentToken with
            | TokenType.EOF -> stream
            | _ -> parseLoop stream newPointer

        parseLoop () 0

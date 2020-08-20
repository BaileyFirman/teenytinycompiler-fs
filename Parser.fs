namespace Parser

open Types.Tokens

module Parser =
    let getToken (tokenStream: Token[]) pointer: Token =
        tokenStream.[pointer]

    let getType (token: Token) = token.Type

    let next pointer = pointer + 1

    let checkTokenType currentTokenType tokenType =
        currentTokenType = tokenType

    let parseTokenStream (tokenStream: Token []) =
        let rec newline streamPointer =
            let nextStreamPointer = next streamPointer
            let nextToken = getToken tokenStream nextStreamPointer
            let nextTokenType = getType nextToken

            match nextTokenType with
            | TokenType.NEWLINE -> newline nextStreamPointer
            | _ -> nextStreamPointer

        let expression = 1

        let rec printStatement streamPointer =
            printfn "PARSE: STATEMENT-PRINT"
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken

            let mainOffset =
                match currentTokenType with
                | TokenType.STRING -> 1
                | _ -> expression

            let newStreamPointer = streamPointer + mainOffset
            newline newStreamPointer

        let comparison streamPointer =
            printfn "PARSE: COMPARISON"
            streamPointer + 3

        let matchThen streamPointer =
            printfn "PARSE: THEN"
            streamPointer + 1

        let labelStatement streamPointer =
            printfn "PARSE: STATEMENT-LABEL"
            let identityToken = getToken tokenStream streamPointer 
            let identityTokenType = getType identityToken

            let newlinePointer =
                match identityTokenType with
                | TokenType.IDENT -> next streamPointer
                | _ -> failwith "EXPECTED IDENTIFIER"

            newline newlinePointer



        let gotoStatement streamPointer =
            printfn "PARSE: STATEMENT-GOTO"
            let identityToken = getToken tokenStream streamPointer 
            let identityTokenType = getType identityToken

            match identityTokenType with
            | TokenType.IDENT -> next streamPointer
            | _ -> failwith "EXPECTED IDENTIFIER"

        let rec ifStatement streamPointer =
            printfn "PARSE: STATEMENT-IF"
            // Lets not handle a comparison
            let comparisonOffsetPointer = comparison streamPointer
            let comparisonOffsetToken = getToken tokenStream comparisonOffsetPointer
            let comparisonOffsetTokenType = getType comparisonOffsetToken

            let thenOffset =
                match comparisonOffsetTokenType with
                | TokenType.THEN -> matchThen comparisonOffsetPointer
                | _ -> failwith "EXPECTED THEN"
            
            let newLineOffset = newline thenOffset

            let rec statementLoop streamPointer =
                let currentToken = getToken tokenStream streamPointer
                let currentTokenType = getType currentToken
                match currentTokenType with
                | TokenType.ENDIF -> streamPointer
                | _ -> statementLoop (statement streamPointer)

            let statementsOffset = statementLoop newLineOffset

            let endToken = getToken tokenStream statementsOffset
            let endTokenType = getType endToken

            match endTokenType with
            | TokenType.ENDIF -> statementsOffset + 1
            | _ -> failwith "EXPECTED ENDIF"
        
        and whileStatement streamPointer =
            printfn "PARSE: STATEMENT-WHILE"
            // Lets not handle a comparison
            let comparisonOffsetPointer = comparison streamPointer
            let comparisonOffsetToken = getToken tokenStream comparisonOffsetPointer
            let comparisonOffsetTokenType = getType comparisonOffsetToken

            let thenOffset =
                match comparisonOffsetTokenType with
                | TokenType.REPEAT -> matchThen comparisonOffsetPointer
                | _ -> failwith "EXPECTED REPEAT"
            
            let newLineOffset = newline thenOffset

            let rec statementLoop streamPointer =
                let currentToken = getToken tokenStream streamPointer
                let currentTokenType = getType currentToken
                match currentTokenType with
                | TokenType.ENDWHILE -> streamPointer
                | _ -> statementLoop (statement streamPointer)

            let statementsOffset = statementLoop newLineOffset

            let endToken = getToken tokenStream statementsOffset
            let endTokenType = getType endToken

            match endTokenType with
            | TokenType.ENDWHILE -> statementsOffset + 1
            | _ -> failwith "EXPECTED ENDIF"

        and statement streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let nextStreamPointer = next streamPointer
            
            match currentTokenType with
            | TokenType.PRINT -> printStatement nextStreamPointer
            | TokenType.IF -> ifStatement nextStreamPointer
            | TokenType.WHILE -> whileStatement nextStreamPointer
            | TokenType.LABEL -> labelStatement nextStreamPointer
            | TokenType.GOTO -> gotoStatement nextStreamPointer
            | _ -> failwith ("NOT IMPLEMENTED: " + (currentTokenType.ToString()))

        let rec parseLoop streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let isEndOfStream = checkTokenType currentTokenType TokenType.EOF

            match isEndOfStream with
            | true -> 0
            | false ->
                streamPointer
                |> statement
                |> parseLoop

        printfn "PARSE: START PARSING"
        parseLoop 0 |> ignore
        printfn "PARSE: END PARSING"

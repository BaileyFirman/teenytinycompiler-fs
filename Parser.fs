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
            printfn "PARSE: NEWLINE"
            let nextStreamPointer = next streamPointer
            let nextToken = getToken tokenStream nextStreamPointer
            let nextTokenType = getType nextToken

            match nextTokenType with
            | TokenType.NEWLINE -> newline nextStreamPointer
            | _ -> nextStreamPointer

        let primary streamPointer =
            let currentToken = getToken tokenStream streamPointer
            printfn "PARSE: PRIMARY \"%s\"" currentToken.Text

            match currentToken.Type with
            | TokenType.NUMBER | TokenType.IDENT -> next streamPointer
            | _ -> failwith ("UNEXPECTED TOKEN" + currentToken.Text)

        let unary streamPointer =
            printfn "PARSE: UNARY"
            let optionalToken = getToken tokenStream streamPointer
            
            let newOffset =
                match optionalToken.Type with
                | TokenType.PLUS | TokenType.MINUS -> next streamPointer
                | _ -> streamPointer  

            primary newOffset

        let term streamPointer =
            printfn "PARSE: TERM"
            let unaryOffset = unary streamPointer

            let rec unaryLoop pointer =
                let currentToken = getToken tokenStream pointer
                match currentToken.Type with
                | TokenType.ASTERISK | TokenType.SLASH ->
                    let nextPointer = next pointer
                    let nextUnaryOffset = unary nextPointer
                    unaryLoop nextUnaryOffset 
                | _ -> pointer
            unaryLoop unaryOffset

        let expression streamPointer =
            printfn "PARSE: EXPRESSION"
            let termOffset = term streamPointer

            let rec termLoop pointer =
                let currentToken = getToken tokenStream pointer
                match currentToken.Type with
                | TokenType.PLUS | TokenType.MINUS ->
                    let nextPointer = next pointer
                    let nextTermOffset = term nextPointer
                    termLoop nextTermOffset
                | _ -> pointer
            termLoop termOffset

        let rec printStatement streamPointer =
            printfn "PARSE: STATEMENT-PRINT"
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken

            let mainOffset =
                match currentTokenType with
                | TokenType.STRING -> 1
                | _ -> expression streamPointer

            let newStreamPointer = streamPointer + mainOffset
            newline newStreamPointer

        let comparison streamPointer =
            printfn "PARSE: COMPARISON"
            let operatorPointer = expression streamPointer
            let operatorToken = getToken tokenStream operatorPointer
            let operatorTokenType = getType operatorToken

            let nextPointer =
                match isComparisonOperator operatorTokenType with
                | true ->
                    let expressionPointer = next operatorPointer
                    expression expressionPointer
                | false -> failwith "EXPECTED A COMPARISION OPERATOR"

            let rec additionalComparisonLoop pointer =
                let nextToken = getToken tokenStream pointer
                let nextTokenType = getType nextToken
                match isComparisonOperator nextTokenType with
                | true ->
                    pointer |> next |> expression |> additionalComparisonLoop
                | _ -> pointer

            additionalComparisonLoop nextPointer

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

        let letStatement streamPointer =
            printfn "PARSE: STATEMENT-LET"
            let identityToken = getToken tokenStream streamPointer 
            let identityTokenType = getType identityToken

            let assignmentTokenPointer =
                match identityTokenType with
                | TokenType.IDENT -> next streamPointer
                | _ -> failwith "EXPECTED IDENTIFIER"

            let expressionToken = getToken tokenStream assignmentTokenPointer 
            let expressionTokenType = getType expressionToken

            let expressionPointer =
                match expressionTokenType with
                | TokenType.EQ -> next assignmentTokenPointer
                | _ -> failwith "EXPECTED EQ"

            let newlinePointer = expression expressionPointer
            newline newlinePointer

        let gotoStatement streamPointer =
            printfn "PARSE: STATEMENT-GOTO"
            let identityToken = getToken tokenStream streamPointer 
            let identityTokenType = getType identityToken

            match identityTokenType with
            | TokenType.IDENT -> next streamPointer
            | _ -> failwith "EXPECTED IDENTIFIER"

        let inputStatement streamPointer =
            printfn "PARSE: STATEMENT-INPUT"
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
            | TokenType.LET -> letStatement nextStreamPointer
            | TokenType.INPUT -> inputStatement nextStreamPointer
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

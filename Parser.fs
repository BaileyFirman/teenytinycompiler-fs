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
            printfn "PARSE: STATEMENT-NEWLINE"
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

        let comparison () =
            printfn "PARSE: COMPARISON"
            3

        let rec ifStatement streamPointer =
            printfn "PARSE: STATEMENT-IF"
            let comparisonOffsetPointer = streamPointer + comparison ()
            let comparisonOffsetToken = getToken tokenStream comparisonOffsetPointer
            let comparisonOffsetTokenType = getType comparisonOffsetToken

            let thenOffset =
                match comparisonOffsetTokenType with
                | TokenType.THEN -> comparison ()
                | _ -> failwith "EXPECTED THEN"
            
            let newLineOffset = newline (thenOffset + 1)

            let rec statementLoop streamPointer =
                let currentToken = getToken tokenStream streamPointer
                let currentTokenType = getType currentToken
                match currentTokenType with
                | TokenType.ENDIF -> next streamPointer
                | _ ->
                    statementLoop (statement streamPointer)

            let statementsOffset = statementLoop newLineOffset

            let endToken = getToken tokenStream statementsOffset
            let endTokenType = getType endToken

            match endTokenType with
            | TokenType.ENDIF -> statementsOffset + 1
            | _ -> failwith "EXPECTED ENDIF"

        and statement streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let nextStreamPointer = next streamPointer
            
            match currentTokenType with
            | TokenType.PRINT -> printStatement nextStreamPointer
            | TokenType.IF -> ifStatement nextStreamPointer
            | _ -> failwith "NOT IMPLEMENTED"

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

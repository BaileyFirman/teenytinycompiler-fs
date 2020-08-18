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
            // We need to assume newline is passed a
            // pointer to the fist newline in a series
            printfn "PARSE: STATEMENT-NEWLINE"
            let nextStreamPointer = next streamPointer
            let nextToken = getToken tokenStream nextStreamPointer
            let nextTokenType = getType nextToken

            match nextTokenType with
            | TokenType.NEWLINE -> newline nextStreamPointer
            | _ -> nextStreamPointer

        let expression = 1

        let rec printStatement streamPointer =
            // We know the previous token was print
            // becuase we called into printStatement
            printfn "PARSE: STATEMENT-PRINT"
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken

            let mainOffset =
                match currentTokenType with
                | TokenType.STRING -> 1
                | _ -> expression

            let newStreamPointer = streamPointer + mainOffset
            newline newStreamPointer

        let rec statement streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let nextStreamPointer = next streamPointer
            
            match currentTokenType with
            | TokenType.PRINT -> printStatement nextStreamPointer
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

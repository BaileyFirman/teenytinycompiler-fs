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
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let nextPointer = next streamPointer

            match currentTokenType with
            | TokenType.NEWLINE -> newline nextPointer
            | _ -> nextPointer

        let expression = 1

        let rec printStatement streamPointer =
            // We know the previous token was print
            // becuase we called into printStatement
            printf "PARSE: STATEMENT-PRINT"
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken

            let mainOffset =
                match currentTokenType with
                | TokenType.STRING -> 1
                | _ -> expression
            mainOffset

        let rec statement streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken

            match currentTokenType with
            | TokenType.PRINT -> printStatement
            | _ -> failwith "NOT IMPLEMENTED"

        let rec parseLoop streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let isEndOfStream = checkTokenType currentTokenType TokenType.EOF

            match isEndOfStream with
            | true -> failwith "NO PARSING IMPLEMENTED"
            | false -> statement streamPointer

        parseLoop 0

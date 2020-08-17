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
        let statement streamPointer =
            0    

        let rec parseLoop streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let currentTokenType = getType currentToken
            let isEndOfStream = checkTokenType currentTokenType TokenType.EOF

            match isEndOfStream with
            | true -> failwith "NO PARSING IMPLEMENTED"
            | false -> statement streamPointer

        parseLoop 0

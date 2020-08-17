namespace Parser

open Types.Tokens

module Parser =
    let getToken (tokenStream: Token[]) pointer: Token =
        tokenStream.[pointer]

    let getType (token: Token) = token.Type

    let next pointer = pointer + 1

    let checkToken token tokenType =
        token.Type = tokenType 

    let parseTokenStream (tokenStream: Token []) =
        let rec parseLoop streamPointer =
            let currentToken = getToken tokenStream streamPointer
            let isEndOfFile =
                currentToken
                |> getType
                |> checkToken TokenType.EOF

            match isEndOfFile with
            | _ -> failwith "NO PARSING IMPLEMENTED" 

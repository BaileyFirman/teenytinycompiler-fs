namespace Parser

open Types.Tokens

module BetterParser =
    let private tokenKind token = token.Type
    let private next pointer = pointer + 1
    let private dummyToken = { Text = ""; Type = TokenType.EOF }

    let parseTokenStream (tokenStream: Token []) =
        let getToken pointer = tokenStream.[pointer]

        let rec parseLoop (stream: unit) streamPointer =
            let currentToken = getToken streamPointer

            let currentType = tokenKind currentToken
            printf "PARSE: %s\n" <| currentType.ToString()

            let nextToken =
                match tokenKind currentToken with
                | TokenType.EOF -> dummyToken
                | _ -> streamPointer |> next |> getToken
            
            let newPointer = next streamPointer

            match tokenKind currentToken with
            | TokenType.EOF -> stream
            | _ -> parseLoop stream newPointer 

        parseLoop () 0

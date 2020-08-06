namespace Parser

open Types.Tokens

module Parser =
    let checkToken current kind = kind = current.Type

    let checkPeek peek kind = kind = peek.Type

    let matchToken current kind =
        match checkToken current kind with
        | true -> true
        | false ->
            "Expected"
            + kind.ToString()
            + ", got "
            + current.Type.ToString()
            |> failwith

    let abort message = sprintf "Error. %s" message |> failwith

    let rec skipNewLines (tokens: Token []) count =
        printfn "NEWLINE"
        match checkToken tokens.[0] TokenType.NEWLINE with
        | true -> count
        | false -> skipNewLines tokens.[1..] count + 1

    let private printString (tokens: Token []) =
        let tokenText = tokens.[1].Text
        printf "STATEMENT-PRINT: %s\n" tokenText

        match tokens.[2..].Length = 0 with
        | true -> 2
        | false -> skipNewLines tokens.[2..] 3

    let private printExpression tokens = 0

    let statement (tokens: Token []) =
        let firstToken = tokens.[0]
        let secondToken = tokens.[1]

        match (firstToken.Type, secondToken.Type) with
        | (TokenType.PRINT, TokenType.STRING) -> printString tokens
        // | (TokenType.PRINT, _) -> printExpression tokens
        | _ -> "OPPS" |> failwith

namespace Parser

open Types.Tokens

module Parser =
    let checkToken current kind = kind = current.Type

    let checkPeek peek kind = kind = peek.Type

    let matchToken current kind =
        match checkToken current kind with
        | true -> true
        | false -> "Expected" + kind.ToString() + ", got " + current.Type.ToString() |> failwith


    let abort message = sprintf "Error. %s" message |> failwith

    let rec skipNewLines (tokens: Token []) count =
        printfn "NEWLINE"
        match checkToken tokens.[0] TokenType.NEWLINE with
        | true -> skipNewLines tokens.[1..] count + 1
        | false -> count

    let private printString (tokens: Token []) =
        // We need to get a newline offset
        let z = string tokens.[1].Text
        printfn "%A %d" z z.Length
        printf "STATEMENT-PRINT: %s\n" z
        let trailingNewlinesCount = skipNewLines tokens.[2..] 1
        trailingNewlinesCount + 2

    let private printExpression tokens = 0

    let statement (tokens: Token []) = 
        let first = tokens.[0]
        let second = tokens.[1]

        match (first.Type, second.Type) with
        | (TokenType.PRINT, TokenType.STRING) -> printString tokens
        // | (TokenType.PRINT, _) -> printExpression tokens
        | _ -> "OPPS" |> failwith
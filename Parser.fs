namespace Parser

open Types.Tokens

module Parser =
    let checkToken current kind = kind = current.Type

    let checkPeek peek kind = kind = peek.Type

    let matchToken current kind =
        match checkToken current kind with
        | true -> true
        | false -> false

    let nextToken = 0

    let abort message = sprintf "Error. %s" message |> failwith

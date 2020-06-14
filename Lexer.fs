namespace Lexer

module Lexer =
    let nextChar (tokenArray: char[]): char =
        tokenArray.[0]

    let peek (tokenArray: char[]): char =
        match tokenArray.Length = 1 with
        | true -> '0'
        | false -> tokenArray.[1]

    let abort message =
        printf "%s" message
namespace Parser

open Types.Tokens

module BetterParser =
    let parseTokenStream tokenStream =
        let rec parseLoop tokenStream streamPointer =
            0
        parseLoop tokenStream 0
open Lexer.LexerFuncs
open Types.Tokens
open Parser

[<EntryPoint>]
let main argv =
    let rec parseLoop (inputArray: char[]) (tokens: Token []) =
        let removedWhitespace = skipWhiteSpace inputArray

        let removedComments =
            match removedWhitespace.[0] with
            | '#' -> skipComment removedWhitespace
            | _ -> removedWhitespace
        
        let characterArray = skipWhiteSpace removedComments 

        let currentToken = getToken characterArray
        let skipOffset = skipOffset currentToken
        let nextChar = peek characterArray
        let newTokens = Array.append tokens [| currentToken |]
        
        match nextChar with 
        | '\u0004' -> tokens
        | _ -> parseLoop characterArray.[skipOffset..] newTokens

    // let testStrings = [
    //     // "LET foobar = 123";
    //     // "+- */\n";
    //     // "+- */ >>= #a comment\n= !=\n0";
    //     // "+- # This is a comment!\n */";
    //     // "+- \"This is a string\" # This is a comment!\n */";
    //     // "+-123 9.8654*/";
    //     "IF+-123 foo*THEN/\n"
    // ]

    let testString =
        "PRINT \"hello, world!\"\nPRINT \"hello, world!\"\nPRINT \"hello, world!\"\u0004"

    let parseString (str: string) =
        let arr = str.ToCharArray()
        let tokens = parseLoop arr [||]
        tokens
        |> Array.map (fun x -> x.Type)
        |> printfn "%A"
        tokens

    let tokens = parseString testString

    let rec parseTokens (inputTokens: Token []) =
        let skipCount = Parser.statement inputTokens
        // printfn "%d%d" skipCount inputTokens.Length
        match skipCount >= inputTokens.Length with
        | true -> "" |> failwith
        | _ -> parseTokens inputTokens.[skipCount..]

    parseTokens tokens
    
    0
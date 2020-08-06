open Lexer.LexerFuncs
open Lexer.BetterLexer
open Types.Tokens
open Parser
open Microsoft.FSharp.Core

[<EntryPoint>]
let main argv =
    // printfn "%s" ">>> PROGRAM START"
    let lex2 (inputArray: char[]): Token[] = lex inputArray

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
        // printfn ">>> TOKENS"
        tokens
        |> Array.map (fun x -> x.Type) |> ignore
        // |> printfn ">>> %A"
        tokens

    let test = "+- \"This is a string\" # This is a comment!\n */\n\u0004".ToCharArray()

    // testing only

    let newTokens =
        lex2 test
        |> Array.filter (fun x -> not (x.Type = TokenType.WHITESPACE || x.Type = TokenType.COMMENT))
        |> Array.map (fun x ->
            let text =
                match x.Text with
                | "\n" -> "nl"
                | _ -> x.Text

            let tokenType = x.Type.ToString()
            printf "BETTER: %s\n" tokenType)

    let tokens = parseString testString

    let rec parseTokens (inputTokens: Token []) =
        let skipCount = Parser.statement inputTokens
        // printfn "%d%d" skipCount inputTokens.Length
        match skipCount >= inputTokens.Length with
        | true -> ">>> PROGRAM END"
        | _ -> parseTokens inputTokens.[skipCount..]

    // let final = parseTokens tokens
    // printf "%s" final
    0
open Lexer

[<EntryPoint>]
let main argv =
    let rec parseLoop (inputArray: char[]) (tokens: Token []) =
        let removedWhitespace = Lexer.skipWhiteSpace inputArray

        let removedComments =
            match removedWhitespace.[0] with
            | '#' -> Lexer.skipComment removedWhitespace
            | _ -> removedWhitespace
        
        let characterArray = Lexer.skipWhiteSpace removedComments 

        let currentToken = Lexer.getToken characterArray
        let skipOffset = Lexer.skipOffset currentToken
        let nextChar = Lexer.peek characterArray
        let newTokens = Array.append tokens [| currentToken |]
        
        match nextChar with 
        | '\u0004' -> tokens
        | _ -> parseLoop characterArray.[skipOffset..] newTokens

    let testStrings = [|
        // "LET foobar = 123";
        // "+- */\n";
        // "+- */ >>= #a comment\n= !=\n0";
        // "+- # This is a comment!\n */";
        // "+- \"This is a string\" # This is a comment!\n */";
        // "+-123 9.8654*/";
        "IF+-123 foo*THEN/\n"
    |]

    let parseString (str: string): bool =
        let arr = str.ToCharArray()
        printf "\n%s\n" str
        parseLoop arr [||]
        |> Array.map (fun x -> x.Type)
        |> printfn "%A"
        false

    testStrings
    |> Array.forall parseString
    |> ignore

    0
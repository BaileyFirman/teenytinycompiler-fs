open Lexer

[<EntryPoint>]
let main argv =
    let printc char =
        printf "%c\n" char

    let prints string =
        printf "%s\n" string

    let printt (token: Token) =
        let tokenTypeString = token.Type.ToString()
        printf "%s\n" tokenTypeString

    let rec parseLoop (inputArray: char[]) =
        let currToken =
            inputArray
            |> Lexer.nextChar

        let x =
            currToken
            |> Lexer.getToken

        printt x

        let nextChar = Lexer.peek(inputArray)
        let offset = Lexer.skipWhiteSpace nextChar
        match nextChar with 
        | '0' -> printf ">>> %s" "DONE"
        | _ -> parseLoop (inputArray.[offset..])

    // let input = "LET foobar = 123".ToCharArray()
    let input = "+- */\n".ToCharArray()

    parseLoop input
    0
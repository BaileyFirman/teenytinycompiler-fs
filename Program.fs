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

        let y = Lexer.getToken(currToken, inputArray)

        printt y

        let nextChar = Lexer.peek(inputArray)
        let offset = Lexer.skipOffset(currToken, nextChar)
        
        let newInputArray =
            match inputArray.Length < offset with
            | true -> [||]
            | false -> inputArray.[offset..]

        match nextChar with 
        | '0' -> printf ">>> %s" "DONE"
        | _ ->
            match newInputArray.Length = 0 with
            | true -> printf ">>> %s" "DONE"
            | false -> parseLoop (newInputArray)

    // let input = "LET foobar = 123".ToCharArray()
    // let input = "+- */\n".ToCharArray()
    let input = "+- */ >>= = !=".ToCharArray()

    parseLoop input
    0
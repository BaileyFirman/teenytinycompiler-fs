open Lexer

[<EntryPoint>]
let main argv =
    let printc char =
        printf "%c\n" char

    let prints string =
        printf "%s\n" string

    let rec parseLoop (inputArray: char[]) =
        let currToken =
            inputArray
            |> Lexer.nextChar
            |> Lexer.getToken
        
        let currTokenType = currToken.Type.ToString()

        prints currTokenType

        let nextChar = Lexer.peek(inputArray)
        match nextChar with 
        | '0' -> printf ">>> %s" "DONE"
        | _ -> parseLoop (inputArray.[1..]) 

    // let input = "LET foobar = 123".ToCharArray()
    let input = "+- */".ToCharArray()

    parseLoop input
    0
open Lexer

let printc char =
    printf "%c\n" char

let prints string =
    printf "%s\n" string

let printt (token: Token) =
    let tokenTypeString = token.Type.ToString()
    printf "%s\n" tokenTypeString

let printArray (array: char[]) =
    let arrayAsString = System.String (array)
    printf "\t\t\t\t DEBUG >>> %s\n" arrayAsString

[<EntryPoint>]
let main argv =

    let rec parseLoop (inputArray: char[]) =
        // Skip until the next character isn't whitespace
        let characterArray = Lexer.skipWhiteSpace inputArray
        let currentChar = Lexer.nextChar characterArray
        let nextChar = Lexer.peek characterArray

        let skipOffset = Lexer.skipOffset (currentChar, nextChar)

        Lexer.getToken (currentChar, characterArray)
        |> printt
        |> ignore

        match nextChar with 
        | '0' -> printf "\t\t\t\t DEBUG >>> %s" "DONE"
        | _ -> parseLoop (characterArray.[skipOffset..])

    // let input = "LET foobar = 123".ToCharArray()
    // let input = "+- */\n".ToCharArray()
    let input = "+- */ >>= = !=\n0".ToCharArray()

    parseLoop input
    0
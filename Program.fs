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
        let removedWhitespace = Lexer.skipWhiteSpace inputArray
        // Remove comments
        let removedComments =
            match removedWhitespace.[0] with
            | '#' -> Lexer.skipComment removedWhitespace
            | _ -> removedWhitespace
        
        let characterArray = Lexer.skipWhiteSpace removedComments 

        // Get our current and next chars
        let currentChar = Lexer.nextChar characterArray
        let nextChar = Lexer.peek characterArray

        let currentToken = Lexer.getToken (currentChar, characterArray)
        
        currentToken
        |> printt
        |> ignore

        // How far should we skip?
        let skipOffset = Lexer.skipOffset (currentToken)

        match nextChar with 
        | '\u0004' -> printf "\t\t\t\t DEBUG >>> %s" "DONE"
        | _ -> parseLoop (characterArray.[skipOffset..])

    let testStrings = [|
        "LET foobar = 123";
        "+- */\n";
        "+- */ >>= #a comment\n= !=\n0";
        "+- # This is a comment!\n */";
        "+- \"This is a string\" # This is a comment!\n */";
        "+-123 9.8654*/";
        "IF+-123 foo*THEN/\n"
    |]

    let parseString (str: string): bool =
        let arr = str.ToCharArray()
        printf "\n%s\n" str
        parseLoop arr
        true

    testStrings
    |> Array.forall parseString
    |> ignore

    0
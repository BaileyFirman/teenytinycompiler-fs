open Lexer

[<EntryPoint>]
let main argv =
    let printc char =
        printf "%c\n" char

    let rec parseLoop (inputArray: char[]) =
        let currChar = Lexer.nextChar(inputArray)
        printc currChar

        let nextChar = Lexer.peek(inputArray)
        match nextChar with 
        | '0' -> printf ">>> %s" "DONE"
        | _ -> parseLoop (inputArray.[1..]) 

    let input = "LET foobar = 123".ToCharArray()

    parseLoop input
    0
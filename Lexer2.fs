namespace Lexer2

open Microsoft.FSharp.Core
open System
open Types.Tokens

module Lexer2 =
    let lextCharacterStream (characters: char []) =
        let getChar pointer = characters.[pointer]
        let increment number = number + 1
        let decrement number = number - 1

        let singleToken currentChar: Token =
            let tokenType =
                match currentChar with
                | ' '
                | '\t'
                | '\r' -> WHITESPACE
                | '-' -> MINUS
                | '*' -> ASTERISK
                | '/' -> SLASH
                | '\n' -> NEWLINE
                | '+' -> PLUS
                | '\u0004' -> EOF
                | '<' -> LT
                | '>' -> GT
                | '=' -> EQ
                | _ -> failwith "Aborted Lexing"
            { Type = tokenType
              Text = tokenType.ToString() }

        let doubleToken currentChar pointer: Token =
            let nextPointer = increment pointer
            let nextChar =  getChar nextPointer

            let lexDoubleToken currentChar =
                let tokenType =
                    match currentChar with
                    | '<' -> LTEQ
                    | '>' -> GTEQ
                    | '=' -> EQEQ
                    | '!' -> NOTEQ
                    | _ -> failwith "Aborted Lexing"
                { Type = tokenType
                  Text = tokenType.ToString() }

            match nextChar with
            | '=' -> lexDoubleToken currentChar
            | _ -> singleToken currentChar

        let commentToken pointer =
            let nextPointer = increment pointer
            let remainingChars = characters.[nextPointer..]
            let endPointer = remainingChars |> Array.findIndex ((=) '\n') |> decrement
            let commentText = String remainingChars.[..endPointer]
            { Type = COMMENT; Text = commentText }

        let stringToken pointer =
            let nextPointer = increment pointer
            let remainingChars = characters.[nextPointer..]
            let endPointer = remainingChars |> Array.findIndex ((=) '"') |> decrement
            let stringText = String remainingChars.[..endPointer]
            { Type = STRING; Text = stringText }

        let rec lexLoop pointer tokens =
            let currentChar = getChar pointer
            let newToken =
                match currentChar with
                | ' '
                | '\t'
                | '\r'
                | '-'
                | '/'
                | '\n'
                | '+'
                | '\u0004' -> singleToken currentChar
                | '*' -> commentToken pointer
                | '"' -> stringToken pointer
                | _ -> doubleToken currentChar pointer

            let newTokens = newToken :: tokens
            let newPointer =
                match newToken.Type with
                | COMMENT | STRING -> newToken.Text.Length + 2
                | _ -> newToken.Text.Length

            printfn "%s" <| newToken.Type.ToString()
            
            let nextPointer = increment pointer
            let nextChar = getChar nextPointer
            match nextChar with
            | '\u0004' -> newTokens
            | _ -> lexLoop newPointer newTokens
        lexLoop 0 []

namespace Lexer2

open Microsoft.FSharp.Core
open System
open Types.Tokens

module Lexer2 =
    let private eofToken = { Type = EOF; Text = "EOF" }

    let lextCharacterStream (characters: char []) =
        let getChar pointer = characters.[pointer]
        let increment number = number + 1
        let decrement number = number - 1

        let getToken tokenType text =
            { Type = tokenType; Text = text.ToString() }

        let singleToken currentChar =
            let tokenType =
                match currentChar with
                | ' ' | '\t'| '\r' -> WHITESPACE
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

            getToken tokenType currentChar

        let doubleToken currentChar =
            let tokenType =
                match currentChar with
                | '<' -> LTEQ
                | '>' -> GTEQ
                | '=' -> EQEQ
                | '!' -> NOTEQ
                | _ -> failwith "Aborted Lexing"

            getToken tokenType currentChar

        let streamToken pointer tokenType endChar =
            let nextPointer = increment pointer
            let remainingChars = characters.[nextPointer..]

            let endPointer =
                remainingChars
                |> Array.findIndex ((=) endChar)
                |> decrement

            let streamText = String remainingChars.[..endPointer]
            getToken tokenType streamText

        let commentToken pointer = streamToken pointer COMMENT '\n'
        let stringToken pointer = streamToken pointer STRING '"'

        let symbolToken pointer =
            let nextPointer = increment pointer
            let nextChar = getChar nextPointer

            match getChar pointer with
            | ' ' | '-' | '*' | '/' | '\n' | '\r' | '\t' | '+'
            | '\u0004' as c -> singleToken c
            | '#' -> commentToken pointer
            | '"' -> stringToken pointer
            | c -> match nextChar with
                   | '=' -> doubleToken c
                   | _ -> singleToken c

        let wordToken pointer =
            let rec wordTokenLoop currentPointer =
                let currentChar = getChar currentPointer
                match Char.IsLetter currentChar with
                | true ->
                    let nextPointer = increment currentPointer
                    wordTokenLoop nextPointer
                | _ ->
                    let closingPointer = decrement currentPointer
                    String characters.[pointer..closingPointer]

            let wordText = wordTokenLoop pointer
            let stringType = matchIdentifier wordText
            getToken stringType wordText

        let numberToken pointer =
            let rec newNumberTokenLoop endPointer =
                let nextPointer = increment endPointer
                let nextCharacter = characters.[endPointer]

                let handlePoint endPointer =
                    let nextChar = characters.[increment endPointer]
                    match Char.IsDigit nextChar with
                    | true -> endPointer |> increment |> newNumberTokenLoop
                    | _ -> failwith "Illegal Character in number"

                match Char.IsDigit nextCharacter with
                | true -> newNumberTokenLoop nextPointer
                | _ -> match nextCharacter with
                       | '.' -> handlePoint endPointer
                       | _ -> decrement endPointer

            let pe = pointer |> increment |> newNumberTokenLoop
            let numberText = String characters.[pointer..pe]
            getToken NUMBER numberText

        let rec lexLoop pointer tokens =
            let currentChar = getChar pointer

            let tokenFunc =
                match Char.IsLetterOrDigit currentChar with
                | true -> match Char.IsLetter currentChar with
                          | true -> wordToken
                          | _ -> numberToken
                | _ -> symbolToken

            let newToken = tokenFunc pointer
            let newTokens =
                match newToken.Type with
                | WHITESPACE -> tokens
                | _ ->
                    printfn "LEX2: %s" <| newToken.Type.ToString()
                    newToken :: tokens

            let tokenTypeOffset =
                match newToken.Type with
                | COMMENT | STRING -> 2
                | _ -> 0

            let newPointer = pointer + newToken.Text.Length + tokenTypeOffset

            match getChar newPointer with
            | '\u0004' -> eofToken :: newTokens
            | _ -> lexLoop newPointer newTokens

        lexLoop 0 []
        |> List.rev
        |> List.toArray

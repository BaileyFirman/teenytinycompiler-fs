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

        let singleToken currentChar =
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
                | _ -> failwith ("Aborted Lexing" + currentChar.ToString())

            { Type = tokenType
              Text = currentChar.ToString() }

        let doubleToken currentChar pointer =
            let nextPointer = increment pointer
            let nextChar = getChar nextPointer

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

        let streamToken pointer tokenType endChar =
            let nextPointer = increment pointer
            let remainingChars = characters.[nextPointer..]

            let endPointer =
                remainingChars
                |> Array.findIndex ((=) endChar)
                |> decrement

            let streamText = String remainingChars.[..endPointer]
            { Type = tokenType; Text = streamText }

        let commentToken pointer = streamToken pointer COMMENT '\n'

        let stringToken pointer = streamToken pointer STRING '"'

        let symbolToken pointer =
            let currentChar = getChar pointer
            match currentChar with
            | ' '
            | '\t'
            | '\r'
            | '-'
            | '/'
            | '\n'
            | '+'
            | '*'
            | '\u0004' -> singleToken currentChar
            | '#' -> commentToken pointer
            | '"' -> stringToken pointer
            | _ -> doubleToken currentChar pointer

        let wordToken pointer =
            let rec wordTokenLoop currentPointer =
                let currentChar = getChar currentPointer
                match Char.IsLetter currentChar with
                | true -> wordTokenLoop <| increment currentPointer
                | false -> decrement currentPointer

            let closingPointer = wordTokenLoop pointer

            let wordText = String characters.[pointer..closingPointer]

            let stringType = matchIdentifier wordText
            { Type = stringType; Text = wordText }

        let numberToken sp =
            let rec newNumberTokenLoop ep: Token =
                let nextPointer = increment ep
                let nextCharacter = characters.[ep]

                let handlePoint ep =
                    let nextCharAfterPointer = characters.[increment ep]
                    match Char.IsDigit nextCharAfterPointer with
                    | true -> ep |> increment |> newNumberTokenLoop
                    | false -> failwith "Illegal Character in number"

                match Char.IsDigit nextCharacter, nextCharacter = '.' with
                | true, _ -> newNumberTokenLoop nextPointer
                | _, true -> handlePoint ep
                | false, false ->
                    { Type = NUMBER; Text = String characters.[sp..ep]}

            newNumberTokenLoop <| increment sp

        let rec lexLoop pointer tokens =
            let currentChar = getChar pointer

            let tokenFunc =
                match Char.IsLetterOrDigit currentChar with
                | false -> symbolToken
                | _ ->
                    match Char.IsLetter currentChar with
                    | true -> wordToken
                    | _ -> numberToken

            let newToken = tokenFunc pointer
            let newTokens = newToken :: tokens

            let tokenTypeOffset =
                match newToken.Type with
                | COMMENT | STRING -> 2
                | _ -> 0

            let newPointer = pointer + newToken.Text.Length + tokenTypeOffset

            printfn "%s" <| newToken.Type.ToString()

            match getChar newPointer with
            | '\u0004' -> eofToken :: newTokens
            | _ -> lexLoop newPointer newTokens

        lexLoop 0 []
        |> List.filter (fun x -> (x.Type <> WHITESPACE))
        |> List.rev
        |> List.toArray

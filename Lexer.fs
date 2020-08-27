namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module Lexer =
    // Character detection
    let private isLetter char = Char.IsLetter char
    let private isDigit char = Char.IsDigit char
    let private isPoint char = char = '.'

    // Pointer manipulation
    let private next pointer = pointer + 1
    let private previous pointer = pointer - 1

    // Data transformation
    let private combineChars first second = string first + string second

    let printTokenStream tokenStream =
        tokenStream
        |> Array.map (fun x ->
            let text =
                match x.Text with
                | "\n" -> "nl"
                | "\t" -> "tab"
                | "\r" -> "return"
                | _ -> x.Text

            let tokenType = x.Type.ToString()
            printf "LEX: %s %s\n" tokenType text
            x)

    // Token creation
    let private tokenFromString tokenType text = { Text = text; Type = tokenType }

    let lexCharacterStream (characterStream: char []): Token [] =

        let rec lexLoop (tokenList: list<Token>) streamPointer =
            let currentCharacter = characterStream.[streamPointer]

            let nextCharacter: char =
                match currentCharacter with
                | '\u0004' -> '\u0004'
                | _ -> characterStream.[next streamPointer]

            let tokenFromRange sp ep tokenType =
                let previousPointer = previous ep
                let streamChars = String characterStream.[sp..previousPointer]
                { Text = streamChars; Type = tokenType }

            let streamCharToken tokenType endChar: Token =
                let nextPointer = next streamPointer
                let remainingStream = characterStream.[nextPointer..]
                let endPointer = remainingStream |> Array.findIndex ((=) endChar)
                let closingPointer = endPointer + nextPointer
                tokenFromRange nextPointer closingPointer tokenType

            let singleCharToken tokenType =
                { Text = string currentCharacter; Type = tokenType }

            let doubleCharToken tokenType =
                let tokenText = String [|currentCharacter; nextCharacter|]
                { Text = tokenText; Type = tokenType }

            let buildSymbolToken (): Token =
                match currentCharacter, nextCharacter with
                | ' ', _
                | '\t', _
                | '\r', _ -> singleCharToken WHITESPACE
                | '-', _ -> singleCharToken MINUS
                | '*', _ -> singleCharToken ASTERISK
                | '/', _ -> singleCharToken SLASH
                | '\"', _ -> streamCharToken STRING '\"'
                | '\n', _ -> singleCharToken NEWLINE
                | '\u0004', _ -> singleCharToken EOF
                | '#', _ -> streamCharToken COMMENT '\n'
                | '+', _ -> singleCharToken PLUS
                | '!', '=' -> doubleCharToken NOTEQ
                | '<', '=' -> doubleCharToken LTEQ
                | '<', _ -> singleCharToken LT
                | '=', '=' -> doubleCharToken EQEQ
                | '=', _ -> singleCharToken EQ
                | '>', '=' -> doubleCharToken GTEQ
                | '>', _ -> singleCharToken GT
                | _, _ -> failwith "Aborted Lexing"

            let buildCharacterToken startPointer =
                let nextPointer = next startPointer

                let streamToToken sp ep =
                    let stringText = String characterStream.[sp..ep]
                    let tokenType = matchIdentifier stringText
                    { Text = stringText; Type = tokenType }

                let rec characterToken startPointer endPointer =
                    let nextCharacter = characterStream.[endPointer]
                    let nextPointer = next endPointer
                    let previousPointer = previous endPointer

                    match isLetter nextCharacter with
                    | true -> characterToken startPointer nextPointer 
                    | false -> streamToToken startPointer previousPointer
                
                characterToken startPointer nextPointer

            let buildNumberToken sp =
                let rec newNumberTokenLoop ep: Token =
                    let nextPointer = next ep
                    let nextCharacter = characterStream.[ep]

                    let handlePoint ep =
                        let nextCharAfterPointer = characterStream.[next ep]
                        match isDigit nextCharAfterPointer with
                        | true -> ep |> next |> newNumberTokenLoop
                        | false -> failwith "Illegal Character in number"

                    match isDigit nextCharacter, isPoint nextCharacter with
                    | false, false -> tokenFromRange sp ep NUMBER
                    | _, true -> handlePoint ep
                    | true, _ -> newNumberTokenLoop nextPointer

                newNumberTokenLoop <| next sp

            let newToken: Token =
                match isLetter currentCharacter, isDigit currentCharacter with
                | false, false -> buildSymbolToken ()
                | true, _ -> buildCharacterToken streamPointer
                | _, true -> buildNumberToken streamPointer

            let newTokens = newToken :: tokenList

            let nextPointer =
                let tokenLength = newToken.Text.Length
                let typeOffset =
                    match newToken.Type with
                    | COMMENT | STRING -> 2
                    | _ -> 0
                streamPointer + tokenLength + typeOffset

            match newToken.Type with
            | EOF -> newTokens
            | _ -> lexLoop newTokens nextPointer

        let result =
            lexLoop [] 0
            |> Seq.filter (fun x -> (x.Type <> WHITESPACE))
            |> Seq.toList
            |> List.rev
            |> List.toSeq
            |> Seq.toArray

        printTokenStream result |> ignore
        result

namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module Lexer =
    let private findChar arr char = arr |> Array.findIndex ((=) char)

    let private blankToken text: TokenType -> Token =
        (fun tokenType -> { Text = text; Type = tokenType })

    // Character detection
    let private isLetter char = Char.IsLetter char
    let private isDigit char = Char.IsDigit char
    let private isPoint char = char = '.'

    // Pointer manipulation
    let private next pointer = pointer + 1
    let private previous pointer = pointer - 1

    // Data transformation
    let private charsToString (chars: char []) = String chars
    let private combineChars first second = string first + string second
    let private appendItem array item = Array.append array [| item |]

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

            let tokenFromRange startPointer endPointer tokenType: Token =
                let previousEndPointer = previous endPointer

                let streamChars =
                    characterStream.[startPointer..previousEndPointer]
                    |> charsToString

                tokenFromString tokenType streamChars

            let streamCharToken tokenType closingCharacter: Token =
                let nextPointer = next streamPointer

                let closingPointer =
                    let remainingStream = characterStream.[nextPointer..]

                    let endPointer =
                        findChar remainingStream closingCharacter

                    endPointer + nextPointer

                tokenFromRange nextPointer closingPointer tokenType

            let singleCharToken tokenType: Token =
                tokenFromString tokenType (string currentCharacter)

            let doubleCharToken tokenType: Token =
                let tokenText =
                    combineChars currentCharacter nextCharacter

                tokenFromString tokenType tokenText

            let buildSymbolToken (): Token =
                match currentCharacter, nextCharacter with
                | ' ', _
                | '\t', _
                | '\r', _ -> singleCharToken WHITESPACE
                | '-', _ -> singleCharToken MINUS
                | '!', '=' -> doubleCharToken NOTEQ
                | '!', _ -> "Expected !=" |> failwith
                | '*', _ -> singleCharToken ASTERISK
                | '/', _ -> singleCharToken SLASH
                | '\"', _ -> streamCharToken STRING '\"'
                | '\n', _ -> singleCharToken NEWLINE
                | '\u0004', _ -> singleCharToken EOF
                | '#', _ -> streamCharToken COMMENT '\n'
                | '+', _ -> singleCharToken PLUS
                | '<', '=' -> doubleCharToken LTEQ
                | '<', _ -> singleCharToken LT
                | '=', '=' -> doubleCharToken EQEQ
                | '=', _ -> singleCharToken EQ
                | '>', '=' -> doubleCharToken GTEQ
                | '>', _ -> singleCharToken GT
                | _, _ ->
                    ("Aborted Lexing" + currentCharacter.ToString())
                    |> failwith

            let buildCharacterToken startPointer: Token =
                let streamToToken sp ep =
                    let stringText =
                        characterStream.[sp..ep] |> charsToString

                    let tokenType = matchIdentifier stringText
                    tokenFromString tokenType stringText

                let rec characterToken startPointer endPointer =

                    let nextCharacter = characterStream.[endPointer]

                    match isLetter nextCharacter with
                    | true -> characterToken startPointer <| next endPointer
                    | false -> streamToToken startPointer <| previous endPointer

                characterToken startPointer <| next startPointer

            let buildNumberToken startPointer: Token =
                let rec newNumberTokenLoop endPointer: Token =
                    let nextCharacter = characterStream.[endPointer]

                    match isDigit nextCharacter, isPoint nextCharacter with
                    | false, false -> tokenFromRange startPointer endPointer NUMBER
                    | _, true ->
                        let nextCharAfterPointer = characterStream.[next endPointer]
                        match isDigit nextCharAfterPointer with
                        | true -> endPointer |> next |> newNumberTokenLoop
                        | false -> failwith "Illegal Character in number"
                    | true, _ -> endPointer |> next |> newNumberTokenLoop

                startPointer |> next |> newNumberTokenLoop

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

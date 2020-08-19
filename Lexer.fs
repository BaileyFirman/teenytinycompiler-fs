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

    // Token creation
    let private tokenFromString tokenType text = { Text = text; Type = tokenType }

    let lexCharacterStream (characterStream: char []): Token [] =

        let rec lexLoop tokens streamPointer: Token [] =
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
                | '\r', _ -> singleCharToken TokenType.WHITESPACE
                | '-', _ -> singleCharToken TokenType.MINUS
                | '!', '=' -> doubleCharToken TokenType.NOTEQ
                | '!', _ -> "Expected !=" |> failwith
                | '*', _ -> singleCharToken TokenType.ASTERISK
                | '/', _ -> singleCharToken TokenType.SLASH
                | '\"', _ -> streamCharToken TokenType.STRING '\"'
                | '\n', _ -> singleCharToken TokenType.NEWLINE
                | '\u0004', _ -> singleCharToken TokenType.EOF
                | '#', _ -> streamCharToken TokenType.COMMENT '\n'
                | '+', _ -> singleCharToken TokenType.PLUS
                | '<', '=' -> doubleCharToken TokenType.LTEQ
                | '<', _ -> singleCharToken TokenType.LT
                | '=', '=' -> doubleCharToken TokenType.EQEQ
                | '=', _ -> singleCharToken TokenType.EQ
                | '>', '=' -> doubleCharToken TokenType.GTEQ
                | '>', _ -> singleCharToken TokenType.GT
                | _, _ -> "Aborted Lexing" |> failwith

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
                    | false, false -> tokenFromRange startPointer endPointer TokenType.NUMBER
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

            let newTokens = appendItem tokens newToken

            let offsetFromToken =
                let textLength = newToken.Text.Length

                let offset =
                    match newToken.Type with
                    | TokenType.COMMENT
                    | TokenType.STRING -> 2
                    | _ -> 0

                textLength + offset

            let pointerOffset =
                let tokenOffset = offsetFromToken
                streamPointer + tokenOffset

            match newToken.Type with
            | TokenType.EOF -> newTokens
            | _ -> lexLoop newTokens pointerOffset

        lexLoop [||] 0

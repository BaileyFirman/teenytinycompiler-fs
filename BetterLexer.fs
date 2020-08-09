namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module BetterLexer =

    // Token creation
    let private tokenFromString tokenType text = { Text = text; Type = tokenType }
    let private tokenFromChar tokenType char = tokenFromString tokenType (string char)
    let private findChar arr char = arr |> Array.findIndex ((=) char)

    // Character detection
    let private isLetter char = Char.IsLetter char
    let private isDigit char = Char.IsDigit char
    let private isPoint char = char = '.'

    // Pointer manipulation
    let private next pointer = pointer + 1
    let private previous pointer = pointer - 1

    // Data transformation
    let private charsToString (chars: char []) = String chars
    let private combineChars (first: char) (second: char) = string first + string second

    let lex (characterStream: char []) =

        let rec lexLoop tokens streamPointer: Token [] =
            let currentCharacter = characterStream.[streamPointer]

            let nextCharacter =
                match currentCharacter with
                | '\u0004' -> '\u0004'
                | _ -> characterStream.[next streamPointer]

            let singleToken tokenType = tokenFromChar tokenType currentCharacter

            let multiToken tokenType =
                let tokenText =
                    combineChars currentCharacter nextCharacter

                tokenFromString tokenType tokenText

            let streamToken tokenType closingCharacter =
                let remainingStream =
                    let nextPointer = next streamPointer
                    characterStream.[nextPointer..]

                let closingPointer =
                    findChar remainingStream closingCharacter

                let tokenText =
                    let previousPointer = previous closingPointer
                    remainingStream.[..previousPointer]
                    |> charsToString

                tokenFromString tokenType tokenText

            let stringToken tokenType = streamToken tokenType '\"'
            let commentToken tokenType = streamToken tokenType '\n'

            let symbolToken () =
                match currentCharacter, nextCharacter with
                | ' ', _
                | '\t', _
                | '\r', _ -> singleToken TokenType.WHITESPACE
                | '-', _ -> singleToken TokenType.MINUS
                | '!', '=' -> multiToken TokenType.NOTEQ
                | '!', _ -> "Expected !=" |> failwith
                | '*', _ -> singleToken TokenType.ASTERISK
                | '/', _ -> singleToken TokenType.SLASH
                | '\"', _ -> stringToken TokenType.STRING
                | '\n', _ -> singleToken TokenType.NEWLINE
                | '\u0004', _ -> singleToken TokenType.EOF
                | '#', _ -> commentToken TokenType.COMMENT
                | '+', _ -> singleToken TokenType.PLUS
                | '<', '=' -> multiToken TokenType.LTEQ
                | '<', _ -> singleToken TokenType.LT
                | '=', '=' -> multiToken TokenType.EQEQ
                | '=', _ -> singleToken TokenType.EQ
                | '>', '=' -> multiToken TokenType.GTEQ
                | '>', _ -> singleToken TokenType.GT
                | _, _ -> "Aborted Lexing" |> failwith

            let buildCharacterToken startPointer =
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

            let buildNumberToken startPointer =
                let rec newNumberTokenLoop endPointer: Token =
                    let nextCharacter = characterStream.[endPointer]

                    match isDigit nextCharacter, isPoint nextCharacter with
                    | false, false ->
                        let tokenText =
                            String characterStream.[startPointer..endPointer |> previous]

                        tokenFromString TokenType.NUMBER tokenText
                    | _, true ->
                        let nextCharAfterPointer = characterStream.[next endPointer]
                        match isDigit nextCharAfterPointer with
                        | true -> endPointer |> next |> newNumberTokenLoop
                        | false -> failwith "Illegal Character in number"
                    | true, _ -> endPointer |> next |> newNumberTokenLoop

                startPointer |> next |> newNumberTokenLoop

            let token =
                match isLetter currentCharacter, isDigit currentCharacter with
                | false, false -> symbolToken ()
                | true, _ -> buildCharacterToken streamPointer
                | _, true -> buildNumberToken streamPointer

            let newTokens = [| token |] |> Array.append tokens

            let pointerOffset =
                match token.Type with
                | TokenType.STRING -> token.Text.Length + 2 // "xxx"
                | TokenType.COMMENT -> token.Text.Length + 1 // #xx/n
                | _ -> token.Text.Length

            match token.Type with
            | TokenType.EOF -> newTokens
            | _ -> lexLoop newTokens <| streamPointer + pointerOffset

        lexLoop [||] 0

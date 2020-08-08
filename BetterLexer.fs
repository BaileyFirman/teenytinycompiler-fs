namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module BetterLexer =

    let private tokenFromString tokenType text = { Text = text; Type = tokenType }
    let private tokenFromChar tokenType char = tokenFromString tokenType (string char)
    let private findChar arr char = arr |> Array.findIndex ((=) char)

    let private isLetter char = Char.IsLetter char
    let private isDigit char = Char.IsDigit char
    let private isPoint char = char = '.'

    let lex (characterStream: char []) =

        let rec lexLoop streamPointer tokens: Token [] =

            let currentCharacter = characterStream.[streamPointer]

            let nextCharacter =
                if currentCharacter = '\u0004' then '\u0004' else characterStream.[streamPointer + 1]

            let singleToken tokenType = tokenFromChar tokenType currentCharacter

            let multiToken tokenType =
                let tokenText =
                    string currentCharacter + string nextCharacter

                tokenFromString tokenType tokenText

            let streamToken tokenType closingCharacter =
                let remainingStream = characterStream.[(streamPointer + 1)..]

                let closingIndex =
                    findChar remainingStream closingCharacter

                tokenFromString tokenType
                <| String remainingStream.[..closingIndex]

            let stringToken tokenType = streamToken tokenType '\"'

            let commentToken tokenType = streamToken tokenType '\n'

            let symbolToken param =
                match currentCharacter, nextCharacter with
                | ' ', _
                | '\t', _
                | '\r', _ -> singleToken TokenType.WHITESPACE
                | '+', _ -> singleToken TokenType.PLUS
                | '-', _ -> singleToken TokenType.MINUS
                | '*', _ -> singleToken TokenType.ASTERISK
                | '/', _ -> singleToken TokenType.SLASH
                | '\n', _ -> singleToken TokenType.NEWLINE
                | '\u0004', _ -> singleToken TokenType.EOF
                | '=', '=' -> multiToken TokenType.EQEQ
                | '=', _ -> singleToken TokenType.EQ
                | '>', '=' -> multiToken TokenType.GTEQ
                | '>', _ -> singleToken TokenType.GT
                | '<', '=' -> multiToken TokenType.LTEQ
                | '<', _ -> singleToken TokenType.LT
                | '!', '=' -> multiToken TokenType.NOTEQ
                | '!', _ -> "Expected !=" |> failwith
                | '\"', _ -> stringToken TokenType.STRING
                | '#', _ -> commentToken TokenType.COMMENT
                | _, _ -> "Aborted Lexing" |> failwith

            let buildCharacterToken startPointer =
                let rec characterToken startPointer endPointer =

                    let nextCharacter = characterStream.[endPointer]

                    match isLetter nextCharacter with
                    | true -> characterToken startPointer (endPointer + 1)
                    | false ->
                        let stringText =
                            String characterStream.[startPointer..endPointer - 1]

                        let tokenType = matchIdentifier stringText
                        tokenFromString tokenType stringText

                characterToken startPointer (startPointer + 1)

            let buildNumberToken startPointer =
                let rec decimalTokenLoop endPointer =
                    let nextCharacter = characterStream.[endPointer]

                    match isDigit nextCharacter with
                    | true -> decimalTokenLoop (endPointer + 1)
                    | false ->
                        let tokenText =
                            String characterStream.[startPointer..endPointer - 1]

                        tokenFromString TokenType.NUMBER tokenText

                let rec numberTokenLoop endPointer: Token =
                    let nextCharacter = characterStream.[endPointer]

                    let isNumber = isDigit nextCharacter
                    match isNumber with
                    | true -> numberTokenLoop (endPointer + 1)
                    | false ->
                        match isPoint nextCharacter with
                        | true -> decimalTokenLoop (endPointer + 1)
                        | false ->
                            let tokenText =
                                String characterStream.[startPointer..endPointer - 1]

                            tokenFromString TokenType.NUMBER tokenText

                numberTokenLoop (startPointer + 1)

            let token =
                match isLetter currentCharacter, isDigit currentCharacter with
                | false, false -> symbolToken ()
                | true, _ -> buildCharacterToken streamPointer
                | _, true -> buildNumberToken streamPointer

            let newTokens = [| token |] |> Array.append tokens

            let pointerOffset =
                match token.Type with
                | TokenType.STRING
                | TokenType.COMMENT
                | _ -> token.Text.Length

            match token.Type with
            | TokenType.EOF -> tokens
            | _ -> lexLoop (streamPointer + pointerOffset) newTokens

        lexLoop 0 [||]

namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module BetterLexer =
    // 1: Pass stream into lexer
    // 2: Recurse with pointer

    let lex (characterStream: char []) =

        let rec lexLoop (streamPointer: int) (tokens: Token []): Token [] =

            let currentCharacter: char = characterStream.[streamPointer]

            let nextCharacter: char =
                match currentCharacter with
                | '\u0004' -> '\u0004'
                | _ -> characterStream.[streamPointer + 1]


            let singleToken (tokenType: TokenType): Token =
                { Text = (string currentCharacter)
                  Type = tokenType }

            let multiToken (tokenType: TokenType): Token =
                { Text = (string currentCharacter) + (string nextCharacter)
                  Type = tokenType }

            let stringToken (tokenType: TokenType) =
                let remainingCharacterStream = characterStream.[(streamPointer + 1)..]

                let closingQuoteIndex =
                    remainingCharacterStream
                    |> Array.findIndex ((=) '\"')

                //printf "remianingStream %s ending %d" (String (remainingCharacterStream)) closingQuoteIndex

                let stringCharsStream =
                    remainingCharacterStream.[..closingQuoteIndex]

                { Text = String(stringCharsStream)
                  Type = tokenType }

            printf "BetterLexer %c %c \n" currentCharacter nextCharacter
            |> ignore

            let token =
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
                | _, _ -> "Aborted Lexing" |> failwith

            let newTokens = tokens |> Array.append [| token |]

            let pointerOffset =
                match token.Type with
                | TokenType.STRING -> token.Text.Length + 1
                | _ -> token.Text.Length

            match token.Type with
            | TokenType.EOF -> tokens
            | _ -> lexLoop (streamPointer + pointerOffset) newTokens

        lexLoop 0 [||]

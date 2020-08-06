namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module BetterLexer =

    let lex (characterStream: char []) =

        let rec lexLoop (streamPointer: int) (tokens: Token []): Token [] =

            let currentCharacter: char = characterStream.[streamPointer]

            let nextCharacter: char =
                if currentCharacter = '\u0004' then '\u0004' else characterStream.[streamPointer + 1]

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

                let stringCharsStream =
                    remainingCharacterStream.[..closingQuoteIndex]

                { Text = String(stringCharsStream)
                  Type = tokenType }

            let commentToken (tokenType: TokenType) =
                let remainingCharacterStream = characterStream.[(streamPointer + 1)..]

                let closingCommentIndex =
                    remainingCharacterStream
                    |> Array.findIndex ((=) '\n')

                let commentCharsStream =
                    remainingCharacterStream.[..closingCommentIndex]

                { Text = String(commentCharsStream)
                  Type = tokenType }

            // printf "BetterLexer %c %c \n" currentCharacter nextCharacter
            // |> ignore

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
                | '#', _ -> commentToken TokenType.COMMENT
                | _, _ -> "Aborted Lexing" |> failwith

            let newTokens = [| token |] |> Array.append tokens

            let pointerOffset =
                match token.Type with
                | TokenType.STRING | TokenType.COMMENT -> token.Text.Length + 1
                | _ -> token.Text.Length

            match token.Type with
            | TokenType.EOF -> tokens
            | _ -> lexLoop (streamPointer + pointerOffset) newTokens

        lexLoop 0 [||]

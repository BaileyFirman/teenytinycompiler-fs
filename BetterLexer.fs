namespace Lexer

open Microsoft.FSharp.Core
open System
open Types.Tokens

module BetterLexer =

    let private tokenFromString text tokenType: Token = { Text = text; Type = tokenType }
    let private tokenFromChar char tokenType: Token = tokenFromString (string char) tokenType
    let private findChar arr char = arr |> Array.findIndex ((=) char)
    let private isLetter char = Char.IsLetter char
    let private isDigit char = Char.IsDigit char
    let private isPoint char = char = '.'

    let lex (characterStream: char []) =

        let rec lexLoop (streamPointer: int) (tokens: Token []): Token [] =

            let currentCharacter: char = characterStream.[streamPointer]

            let nextCharacter: char =
                if currentCharacter = '\u0004' then '\u0004' else characterStream.[streamPointer + 1]

            let singleToken tokenType: Token = tokenFromChar currentCharacter tokenType

            let multiToken tokenType =
                let tokenText =
                    string currentCharacter + string nextCharacter

                tokenFromString tokenText tokenType

            let stringToken tokenType =
                let remainingStream = characterStream.[(streamPointer + 1)..]
                let closingQuoteIndex = findChar remainingStream '\"'

                let stringCharsStream =
                    String remainingStream.[..closingQuoteIndex]

                tokenFromString stringCharsStream tokenType

            let commentToken (tokenType: TokenType) =
                let remainingStream = characterStream.[(streamPointer + 1)..]
                let closingCommentIndex = findChar remainingStream '\"'

                let commentCharsStream =
                    String remainingStream.[..closingCommentIndex]

                tokenFromString commentCharsStream tokenType

            let symbolToken p1 =
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

            // wrap these in a single
            let rec characterToken (startPointer: int) (endPointer: int): Token =

                let tokenType str =
                    match str with
                    | "LABEL" -> TokenType.LABEL
                    | "GOTO" -> TokenType.GOTO
                    | "PRINT" -> TokenType.PRINT
                    | "INPUT" -> TokenType.INPUT
                    | "LET" -> TokenType.LET
                    | "IF" -> TokenType.IF
                    | "THEN" -> TokenType.THEN
                    | "ENDIF" -> TokenType.ENDIF
                    | "WHILE" -> TokenType.WHILE
                    | "REPEAT" -> TokenType.REPEAT
                    | "ENDWHILE" -> TokenType.ENDWHILE
                    | _ -> TokenType.IDENT

                let nextCharacter = characterStream.[endPointer]

                match isLetter nextCharacter with
                | true -> characterToken startPointer (endPointer + 1)
                | false ->
                    let sText =
                        String(characterStream.[startPointer..endPointer - 1])

                    { Type = tokenType sText; Text = sText }

            // wrap in a single
            let buildNumberToken startPointer =
                let rec decimalTokenLoop endPointer: Token =
                    let nextCharacter = characterStream.[endPointer]

                    match isDigit nextCharacter with
                    | true -> decimalTokenLoop (endPointer + 1)
                    | false ->
                        let tokenText =
                            String characterStream.[startPointer..endPointer - 1]

                        tokenFromString tokenText TokenType.NUMBER

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

                            tokenFromString tokenText TokenType.NUMBER

                numberTokenLoop (startPointer + 1)

            let token =
                match isLetter currentCharacter, isDigit currentCharacter with
                | false, false -> symbolToken ""
                | true, _ -> characterToken streamPointer (streamPointer + 1)
                | _, true -> buildNumberToken streamPointer

            let newTokens = [| token |] |> Array.append tokens

            let pointerOffset =
                match token.Type with
                | TokenType.STRING
                | TokenType.COMMENT
                | TokenType.NUMBER -> token.Text.Length + 1
                | _ -> token.Text.Length

            match token.Type with
            | TokenType.EOF -> tokens
            | _ -> lexLoop (streamPointer + pointerOffset) newTokens

        lexLoop 0 [||]

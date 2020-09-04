namespace TennyTiny

open Microsoft.FSharp.Core
open System
open TeenyTiny.Types

module Lexer =
    let private printToken token =
        let typeString = token.Type.ToString()
        printfn "TokenType.%s" typeString

    let private increment number = number + 1
    let private decrement number = number - 1
    let private getToken tokenType text =
        { Type = tokenType; Text = text.ToString() }

    let lex (characters: char []) =
        let getChar pointer = characters.[pointer]

        let singleToken currentChar =
            let tokenType =
                match currentChar with
                | ' ' | '\t'
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
                | _ -> failwith "Aborted Lexing"

            getToken tokenType currentChar

        // Will only be called when the previous char was '='
        let doubleToken currentChar =
            let tokenType =
                match currentChar with
                | '<' -> LTEQ
                | '>' -> GTEQ
                | '=' -> EQEQ
                | '!' -> NOTEQ
                | _ -> failwith "Aborted Lexing"

            getToken tokenType currentChar

        // Everything after the currentCharacter should be searched for the endChar
        let streamToken pointer tokenType endChar =
            let nextPointer = increment pointer
            let trailingChars = characters.[nextPointer..]

            let endPointer =
                trailingChars
                |> Array.findIndex ((=) endChar)
                |> decrement

            let streamText = String trailingChars.[..endPointer]
            getToken tokenType streamText

        let commentToken pointer = streamToken pointer COMMENT '\n'
        let stringToken pointer = streamToken pointer STRING '"'

        let symbolToken pointer =
            let nextPointer = increment pointer
            let nextChar = getChar nextPointer

            // Determine the function to build the token then return the token
            match getChar pointer with
            | ' ' | '-' | '*' | '/'
            | '\n' | '\r' | '\t' | '+'
            | '\u0004' as c -> singleToken c
            | '#' -> commentToken pointer
            | '"' -> stringToken pointer
            | c -> match nextChar with
                   | '=' -> doubleToken c
                   | _ -> singleToken c

        let wordToken pointer =
            let rec buildWordToken currentPointer =
                let currentChar = getChar currentPointer

                // Continue to parse until the current character is not a letter
                match Char.IsLetter currentChar with
                | true -> currentPointer |> increment |> buildWordToken
                | _ ->
                    let closingPointer = decrement currentPointer
                    String characters.[pointer..closingPointer]

            let wordText = buildWordToken pointer
            let stringType = stringToKeywordType wordText
            getToken stringType wordText

        let numberToken pointer =
            let rec callbuildNumberToken pointer =
                pointer
                |> increment
                |> buildNumberToken

            and buildNumberToken endPointer =
                let nextPointer = increment endPointer
                let nextCharacter = characters.[endPointer]

                // Fail if the number is not a valid decimal
                let handlePoint endPointer =
                    let nextChar = characters.[increment endPointer]
                    match Char.IsDigit nextChar with
                    | true -> callbuildNumberToken endPointer
                    | _ -> failwith "Aborted Lexing"

                // Loop over character digits, if a point is hit proceed to handlePoint
                match Char.IsDigit nextCharacter with
                | true -> buildNumberToken nextPointer
                | _ -> match nextCharacter with
                       | '.' -> handlePoint endPointer
                       | _ -> decrement endPointer

            let endPointer = callbuildNumberToken pointer

            let numberText = String characters.[pointer..endPointer]
            getToken NUMBER numberText

        // Depending on the character return the appropriate lexing function
        let getTokenFunction char =
            let charIsLetter = Char.IsLetter char
            let charIsDigit = Char.IsDigit char

            match charIsLetter, charIsDigit with
            | true, _ -> wordToken
            | _, true -> numberToken
            | _, _ -> symbolToken

        let prependToTokens token tokens=
            printToken token
            token :: tokens

        // Account for " " around strings and # \n around comments
        let getOffsetForType token =
            match token.Type with
            | COMMENT | STRING -> 2
            | _ -> 0

        let rec lexCharacters pointer tokens =
            let currentChar = getChar pointer
            let tokenLexFunc = getTokenFunction currentChar
            let newToken = tokenLexFunc pointer

            // Only print and prepend when our new token is not whitespace
            let newTokens =
                match newToken.Type with
                | WHITESPACE -> tokens
                | _ -> prependToTokens newToken tokens

            let tokenTextOffset = newToken.Text.Length
            let tokenTypeOffset = getOffsetForType newToken
            
            // Current Pointer + Characters in lexed token + 2 If our token was a comment or string 
            let newPointer = pointer + tokenTextOffset + tokenTypeOffset 

            // End our lexing when the EOF character is our next character
            match getChar newPointer with
            | '\u0004' -> eofToken :: newTokens
            | _ -> lexCharacters newPointer newTokens

        lexCharacters 0 [] |> List.rev |> List.toArray

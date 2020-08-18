open Lexer.Lexer
open Parser.Parser
open Types.Tokens
open Microsoft.FSharp.Core
open System.IO

[<EntryPoint>]
let main argv =
    let fileAsString = File.ReadAllText argv.[0] + "\u0004"
    let testCharacterStream = fileAsString.ToCharArray()

    let tokenisedStream =
        lexCharacterStream testCharacterStream
        |> Array.map (fun x ->
            let text =
                match x.Text with
                | "\n" -> "nl"
                | _ -> x.Text

            let tokenType = x.Type.ToString()
            printf "LEX: %s %s\n" tokenType text
            x)

    let cleanTokenStream =
        tokenisedStream
        |> Array.filter (fun x -> (x.Type <> TokenType.WHITESPACE))

    parseTokenStream cleanTokenStream
    |> ignore

    0

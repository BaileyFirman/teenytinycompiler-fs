open Lexer.Lexer
open Parser.BetterParser
open Types.Tokens
open Microsoft.FSharp.Core

[<EntryPoint>]
let main argv =
    let testString =
        "#Comment\n\t\r+-*/\n===>=><=< \"A String\" 1234 5.6789 PRINT REPEAT\n\u0004".ToCharArray()

    let tokenisedStream =
        lexCharacterStream testString
        |> Array.map (fun x ->
            let text =
                match x.Text with
                | "\n" -> "nl"
                | _ -> x.Text

            let tokenType = x.Type.ToString()
            printf "LEX: %s %s\n" tokenType text)

    parseTokenStream tokenisedStream
    |> ignore
    
    0

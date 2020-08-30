open Lexer.Lexer
open Microsoft.FSharp.Core
open Parser.Parser
open System.IO
open Lexer2.Lexer2

module FileActions =
    let eofCharacter = '\u0004'.ToString()

    let loadFileString filepath =
        let fileAsString = File.ReadAllText filepath
        fileAsString + eofCharacter

    let writeFileString filepath file =
        File.WriteAllText(filepath, file)
        0

[<EntryPoint>]
let main argv =
    let inputFilePath = argv.[0]
    let outputFilePath =
        match argv.Length with
        | 2 -> argv.[1]
        | _ -> "output.c"

    let fileStream = FileActions.loadFileString inputFilePath
    let tokenStream2 = lextCharacterStream <| fileStream.ToCharArray()
    let tokenStream = lexCharacterStream <| fileStream.ToCharArray()
    let emittedCode = parseTokenStream tokenStream

    FileActions.writeFileString outputFilePath emittedCode
open Microsoft.FSharp.Core
open System.IO
open TennyTiny.Lexer
open TennyTiny.Parser

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
    let tokenStream = lex <| fileStream.ToCharArray()
    let emittedCode = parseTokenStream tokenStream

    FileActions.writeFileString outputFilePath emittedCode
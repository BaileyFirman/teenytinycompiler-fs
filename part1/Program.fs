open Microsoft.FSharp.Core
open System.IO
open TennyTiny.Lexer

module FileActions =
    let eofCharacter = '\u0004'.ToString()

    let loadFileString filepath =
        let fileAsString = File.ReadAllText filepath
        fileAsString + eofCharacter

[<EntryPoint>]
let main argv =
    let inputFilePath = argv.[0]

    let fileStream = FileActions.loadFileString inputFilePath
    let characterStream = fileStream.ToCharArray()
    lex characterStream
    |> ignore
    0

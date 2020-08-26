namespace Emitter

open Microsoft.FSharp.Core
open System.IO

module Emitter =
    type Emitter() =
        let fullpath = "output.c"
        let mutable (outputHeader: List<string>) = []
        let mutable outputCode = ""
        member this.Emit code = outputCode <- outputCode + code
        member this.EmitLine code = outputCode <- outputCode + code + "\n"
        member this.HeaderLine code = outputHeader <- (code + "\n") :: outputHeader
        member this.WriteFile =
            let distinctHeaders = outputHeader |> Seq.distinct |> Seq.toList |> List.rev
            let fileContent = (String.concat "" distinctHeaders)
            printf ">>>>  %s" fileContent
            printfn "%s" (fileContent + outputCode)
            File.WriteAllText(fullpath, (fileContent + outputCode))
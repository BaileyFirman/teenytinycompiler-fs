namespace Emitter

open Microsoft.FSharp.Core
open System.IO

module Emitter =
    type Emitter() =
        let fullpath = "output.c"
        let mutable outputHeader = ""
        let mutable outputCode = ""
        member this.Emit code = outputCode <- outputCode + code
        member this.EmitLine code = outputCode <- outputCode + code + "\n"
        member this.HeaderLine code = outputHeader <- outputHeader + code + "\n"
        member this.WriteFile =
            let fileContent = outputHeader + outputCode
            printfn "%s" fileContent
            File.WriteAllText(fullpath, fileContent)
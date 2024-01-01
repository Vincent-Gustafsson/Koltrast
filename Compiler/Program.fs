module Compiler.Program

open Compiler.Parse
open FParsec
open Microsoft.FSharp.Core

let prog = """
fn foo() -> unit {
    let a = 0
}
"""

[<EntryPoint>]
let main args =
    match runParserOnString Parser.pProgram () "internal" prog with
    | Success(res, _, _) ->
        printfn "%A\n ------------------" res
        match ParseErrorValidator.checkTreeForErrorNodes res with
        | Ok pTree ->
            
        | Error errorValue ->
            printfn $"Number of errors: {errorValue.Length}"
            errorValue |> List.iter (printfn "%s")
    | ParserResult.Failure(s, _, _) -> printfn $"{s}"
    0

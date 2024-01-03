module Compiler.Program

open Compiler.Diagnostics
open Compiler.Parse
open Compiler.Sema

open FParsec
open Microsoft.FSharp.Core

let prog = """
fn foo() -> unit {
    while true {
        let a: i8 = if false then 32 else 78
        a = 127 * 0
    }
}
"""

[<EntryPoint>]
let main args =
    match runParserOnString Parser.pProgram () "internal" prog with
    | Success(res, _, _) ->
        match ParseErrorValidator.checkTreeForErrorNodes res with
        | Ok compUnit ->
            let diagnostics = DiagnosticBag("internal", prog.Split('\n') |> Array.toList)
            let typingRes = Typechecker.typecheckCompUnit diagnostics compUnit
            match typingRes with
            | Ok tCompUnit -> printfn "%s" (tCompUnit.ToString())
            | Error dBag -> List.iter (printfn "%s") (dBag.genDiagnostics())
                
        | Error errorValue ->
            printfn $"Number of errors: {errorValue.Length}"
            errorValue |> List.iter (printfn "%s")
    | ParserResult.Failure(s, _, _) -> printfn $"{s}"
    0

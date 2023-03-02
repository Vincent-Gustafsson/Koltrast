module Program

open Koltrast.Diagnostics
open Koltrast.Frontend.FuncTransform
open Koltrast.Frontend.ValidateEntrypoint
open Koltrast.Frontend.ValidateTopLevel
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.ResultBuilder
open Microsoft.FSharp.Core

let path = @"C:\Users\Vincent Gustafsson\RiderProjects\programmering-kurs\Koltrast\Koltrast\input.txt"
let src = System.IO.File.ReadAllLines(path) |> List.ofSeq
let diagnostics = DiagnosticBag(path)

// PARSE
let compUnit =
    match parseFile path with
    | Ok compUnit -> compUnit
    | Result.Error err -> printfn "%s" err |> exit 1

// SEMANTIC ANALYSIS
//   1. Top-Level Validation: Ensure that the top-level expression only are of the type Entrypoints, Functions and Variables
//
//   2. Entrypoint Validation:
//        Ensure that there is an entrypoint (for now, will accept code without entrypoints later on for modules)
//        and that there's only one.
//
//   3. Typechecking: Self-explanatory.
//
//   4. W.I.P. IR Generation: To be explained.
let compilationResult = result {
    
    let! compUnit' = validateTopLevel diagnostics compUnit
    let! compUnit'' = validateEntrypoint diagnostics compUnit'
    let! typedCompUnit = typeCheck diagnostics compUnit''
    return typedCompUnit
}


match compilationResult with
| Ok typedCompUnit ->
    printfn "%A" typedCompUnit
    printfn "----------------------------"
    (**
        let ir = generateIR tAst
        Map.iter (fun name instrs ->
            printfn $"\n{name}:"
            List.iter (fun instr -> printfn $"   {instr}") instrs
        ) ir
    **)
    
| Error diagnostics -> List.iter (printfn "%s") (diagnostics.genDiagnostics())

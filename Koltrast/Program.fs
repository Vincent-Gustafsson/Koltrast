module Program

open Koltrast.Diagnostics
open Koltrast.Frontend.FuncTransform
open Koltrast.Frontend.AST
open Koltrast.Frontend.IR
open Koltrast.Frontend.ValidateEntrypoint
open Koltrast.Frontend.ValidateTopLevel
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.Frontend.GenerateIR
open Koltrast.ResultBuilder
open Microsoft.FSharp.Core

let path = @"C:\Users\vince\RiderProjects\Koltrast\Koltrast\input.txt"
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
    let! compUnit'' = transformFunctions compUnit'
    let! compUnit''' = validateEntrypoint diagnostics compUnit''
    let! typedCompUnit = typeCheck diagnostics compUnit'''
    return typedCompUnit
}


match compilationResult with
| Ok typedCompUnit ->
    // printfn "%A" typedCompUnit
    printfn "----------------------------"
    let entrypoint, functions = generateIR typedCompUnit
    
    printfn
        $"
-------
Entrypoint: {entrypoint}
-------"
    
    List.iter (fun (fn: Function) ->
        printfn "define %s %s {" (typeToStr fn.ReturnType) fn.Name
        List.iter (fun (b: Block) ->
            printfn $"{b.Name}:"
            List.iter (fun instr ->
                printfn $"  {instr._instr}"
            ) b.Instructions
        ) fn.Blocks
        printfn "}"
    ) functions
    
| Error diagnostics -> List.iter (printfn "%s") (diagnostics.genDiagnostics())

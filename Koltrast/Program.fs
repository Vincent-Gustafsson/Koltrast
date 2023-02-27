module Program

open Koltrast.Diagnostics
open Koltrast.Frontend.FuncTransform
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.Middleend.IR

let path = @"C:\Users\vince\RiderProjects\Koltrast\Koltrast\input.txt"

let src = System.IO.File.ReadAllLines(path) |> List.ofSeq

match parseFile path with
| Ok compUnit ->
    let compUnit' = transformFunctions compUnit
    compUnit' KINDA WORKS, HOWEVER, GENERATES / KEEPS THE ANONFUNC IN OUTER BLOCK???
    // let diagnostics = DiagnosticBag(path)
    (**
    match typeCheck diagnostics compUnit' with
    | Ok tAst ->
        printfn $"{tAst}"
        printfn "----------------------------"
        let ir = generateIR tAst
        Map.iter (fun name instrs ->
            printfn $"{name}:"
            List.iter (fun instr -> printfn $"   {instr}") instrs
        ) ir
    | Result.Error errors ->
        List.iter (printfn "%s") errors
    **)
| Result.Error err -> printfn "%s" err |> exit 1

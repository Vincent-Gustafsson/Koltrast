module Program

open Koltrast.Diagnostics
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.Middleend.IR

let path = @"C:\Users\vince\RiderProjects\Koltrast\Koltrast\input.txt"

let src = System.IO.File.ReadAllLines(path) |> List.ofSeq

match parseFile path with
| Ok compUnit ->
    let diagnostics = DiagnosticBag(path)
    match typeCheck diagnostics compUnit with
    | Ok tAst ->
        printfn $"{tAst}"
        printfn "----------------------------"
        let ir = generateIR tAst
        List.iter (fun instr -> printfn $"  {instr}") ir
    | Result.Error errors ->
        List.iter (printfn "%s") errors
        
    | Result.Error e -> List.iter (printfn "%A") e
| Result.Error err -> printfn "%s" err |> exit 1

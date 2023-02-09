module Program

open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.Interpreter

let path = @"C:\Users\vince\RiderProjects\Koltrast\Koltrast\input.txt"

let src = System.IO.File.ReadAllLines(path) |> List.ofSeq

match parseFile path with
| Ok compUnit ->
    match typeCheckCompilationUnit src compUnit with
    | Ok typedCompUnit ->
        printfn "%A" typedCompUnit
        let env, res = evaluateProgram typedCompUnit
        printfn "__________________________________"
        printfn "VAR ENVIRONMENT"
        Map.iter (fun k v -> printfn "%s => %A" k v) env.vars
        
        printfn "FUNC ENVIRONMENT"
        Map.iter (fun k v -> printfn "%s => BLOCK" k ) env.funcs
        
        printfn "__________________________________"
        printfn "[Result] %A" res
    | Error e -> List.iter (fun err -> printfn "%s" err) e
    
| Error err -> printfn "%s" err |> exit 1

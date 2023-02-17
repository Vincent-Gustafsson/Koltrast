module Program

open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.Interpreter

let path = @"C:\Users\Vincent Gustafsson\RiderProjects\programmering-kurs\Koltrast\Koltrast\input.txt"

let src = System.IO.File.ReadAllLines(path) |> List.ofSeq

match parseFile path with
| Ok compUnit ->
    match typeCheckCompilationUnit src compUnit with
    | Ok typedCompUnit ->
        let env, res = evaluateProgram typedCompUnit
        printfn "[Result] %A" res
    | Error e -> List.iter (fun err -> printfn "%s" err) e |> exit 1
    
| Error err -> printfn "%s" err |> exit 1

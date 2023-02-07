module Program

open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker
open Koltrast.Interpreter

let path = @"C:\Users\Vincent Gustafsson\RiderProjects\programmering-kurs\Koltrast\Koltrast\input.txt"

let src = System.IO.File.ReadAllLines(path) |> List.ofSeq

let pprint compUnit =
    let rec inner stmt =
        match stmt with
        | TypedStmt.AnnVarDecl(_, loc, name, mut, ty, initExprOpt) ->
            $"({name}, {mut}) => {ty} \n"
        | TypedStmt.InferredVarDecl(_, loc, name, mut, tyOpt, initExpr) ->
            $"({name}, {mut}) => {tyOpt} \n"
        | Block(_, _, stmts) ->
            ("", stmts)
            ||> List.fold (fun acc stmt -> acc + "\n" + (inner stmt))
        | ExprStmt(_, _, texpr) -> $"EXPRSTMT {texpr}"

    inner (compUnit |> snd)

match parseFile path with
| Ok compUnit ->
    match checkCompilationUnit src compUnit with
    | Ok typedCompUnit ->
        printfn "%s" <| pprint typedCompUnit
        let (env, _) = evaluateCompUnit typedCompUnit
        
        Map.iter (fun k v -> printfn "%s" $"{k} => {v}") env
        
    | Error e -> List.iter (fun err -> printfn "%s" err) e
    
| Error err -> printfn "%s" err |> exit 1

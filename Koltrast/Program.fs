module Program

open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser
open Koltrast.Frontend.Typechecker

let src = System.IO.File.ReadAllLines(@"C:\Users\vince\RiderProjects\Koltrast\Koltrast\input.txt") |> List.ofSeq

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

match parseFile @"C:\Users\Vincent Gustafsson\RiderProjects\programmering-kurs\Koltrast\Koltrast\input.txt" with
| Ok compUnit ->
    match checkCompilationUnit src compUnit with
    | Ok typedCompUnit -> printfn "%s" <| pprint typedCompUnit
    | Error e -> List.iter (fun err -> printfn "%s" err) e
    
| Error err -> printfn "%s" err |> exit 1

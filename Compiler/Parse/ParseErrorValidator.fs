module Compiler.Parse.ParseErrorValidator

open Compiler.AST.ParsedAST

let rec checkExprForErrorNode expr: string list =
    match expr._expr with
    | Error errStr -> [errStr]
    | BinOp bin ->
        [bin.Left; bin.Right]
        |> List.collect checkExprForErrorNode
    | IfExpr iff ->
        [iff.Cond; iff.Then; iff.Else]
        |> List.collect checkExprForErrorNode
    | FuncAppl appl ->
        appl.Arguments
        |> List.collect checkExprForErrorNode
    | While w ->
        [w.Cond; w.Body]
        |> List.collect checkExprForErrorNode
    |

let checkTreeForErrorNodes compUnit =
    compUnit
    |> List.fold (fun item ->
        match item with
        | ItemKind.Function fn ->
            match checkExprForErrorNode with
            | Some e ->
            | None -> ) List.empty

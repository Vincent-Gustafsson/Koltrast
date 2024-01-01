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
    | Block exprs ->
        exprs
        |> List.collect checkExprForErrorNode
    | LetVar v
    | ConstVar v ->
        [v.Name; v.InitExpr]
        |> List.collect checkExprForErrorNode
    | Assign ass ->
        [ass.Name; ass.AssExpr]
        |> List.collect checkExprForErrorNode
    | _ -> []

let checkTreeForErrorNodes (compUnit: CompilationUnit) =
    (List.empty, compUnit.Items)
    ||> List.fold (fun acc item ->
        match item._item with
        | ItemKind.Function fn -> acc @ (checkExprForErrorNode fn.Body)
        | _ -> acc)
    |> function
        | [] -> Result.Ok(compUnit)
        | errs -> Result.Error errs
    
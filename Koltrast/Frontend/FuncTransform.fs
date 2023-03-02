module Koltrast.Frontend.FuncTransform

open Koltrast.Location
open Koltrast.Frontend.AST

let mutable counter = -1
let genAnonName() =
    counter <- counter + 1
    $"anon_{counter}"

let rec transformFunctions expr =
    match expr._expr with
    | Block exprs ->
        match exprs with
        | [] -> expr
        | [retExpr] ->
            match retExpr._expr with
            | AnonFunc anFn ->
                let fnName = genAnonName()
                let block' = [
                 { _expr=(Func {| Name=fnName; Parameters=anFn.Parameters; TyAnnot=anFn.TyAnnot; Body=anFn.Body |}); Loc=retExpr.Loc; Metadata=() }
                 { _expr=(Ident fnName); Loc=retExpr.Loc; Metadata=() }
                ]
                { _expr=(Block block'); Loc=expr.Loc; Metadata=() }
            | _ -> { _expr=(Block [transformFunctions retExpr]); Loc=expr.Loc; Metadata=() }
        | exprs ->
            let retExpr = List.last exprs
            match retExpr._expr with
            | AnonFunc anFn ->
                let fnName = genAnonName()
                let epi = [
                 { _expr=(Func {| Name=fnName; Parameters=anFn.Parameters; TyAnnot=anFn.TyAnnot; Body=anFn.Body |}); Loc=retExpr.Loc; Metadata=() }
                 { _expr=(Ident fnName); Loc=retExpr.Loc; Metadata=() }
                ]
                
                let exprs' = exprs[..exprs.Length-1] |> List.map (fun e -> transformFunctions e)
                { _expr=(Block (exprs' @ epi)); Loc=expr.Loc; Metadata=() }
            | _ -> 
                let exprs' = exprs |> List.map (fun e -> transformFunctions e)
                { _expr=(Block exprs'); Loc=expr.Loc; Metadata=() }
                
    | Func fn ->
        let body' = transformFunctions fn.Body 
        { _expr=(Func {| Name=fn.Name; Body=body'; Parameters=fn.Parameters; TyAnnot=fn.TyAnnot |}); Loc=expr.Loc; Metadata=() }
    | Var v ->
        match v.InitExprOpt with
        | Some initExpr ->
            match initExpr._expr with
            | AnonFunc anFn ->
                let body' = transformFunctions anFn.Body 
                { _expr=(Func {| Name=v.Name; Body=body'; Parameters=anFn.Parameters; TyAnnot=anFn.TyAnnot |}); Loc=expr.Loc; Metadata=() }
            | _ -> expr
        | None -> expr
    | _ -> expr
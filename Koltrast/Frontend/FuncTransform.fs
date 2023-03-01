module Koltrast.Frontend.FuncTransform

open Koltrast.Location
open Koltrast.Frontend.AST

let mutable counter = -1
let genAnonName() =
    counter <- counter + 1
    $"anon_{counter}"

let rec transformFunction (expr: TypedExpr) =
    match expr._expr with
    | Block exprs ->
        match exprs with
        | [] -> expr
        | [retExpr] ->
            match retExpr._expr with
            | AnonFunc anFn ->
                let fnName = genAnonName()
                let block' = [
                 { _expr=(Func {| Name=fnName; Parameters=anFn.Parameters; TyAnnot=anFn.TyAnnot; Body=anFn.Body |}); Loc=retExpr.Loc; Metadata=retExpr.Metadata }
                 { _expr=(Ident fnName); Loc=retExpr.Loc; Metadata=retExpr.Metadata }
                ]
                { _expr=(Block block'); Loc=expr.Loc; Metadata=retExpr.Metadata }
            | _ -> { _expr=(Block [transformFunction retExpr]); Loc=expr.Loc; Metadata=retExpr.Metadata }
        | exprs ->
            let retExpr = List.last exprs
            match retExpr._expr with
            | AnonFunc anFn ->
                let fnName = genAnonName()
                let epi = [
                 { _expr=(Func {| Name=fnName; Parameters=anFn.Parameters; TyAnnot=anFn.TyAnnot; Body=anFn.Body |}); Loc=retExpr.Loc; Metadata=retExpr.Metadata }
                 { _expr=(Ident fnName); Loc=retExpr.Loc; Metadata=retExpr.Metadata }
                ]
                let exprs' = exprs[..exprs.Length-2] |> List.map (fun e -> transformFunction e)
                { _expr=(Block (exprs' @ epi)); Loc=expr.Loc; Metadata=retExpr.Metadata }
            | _ -> 
                let exprs' = exprs |> List.map (fun e -> transformFunction e)
                { _expr=(Block exprs'); Loc=expr.Loc; Metadata=retExpr.Metadata }
                
    | Func fn ->
        let body' = transformFunction fn.Body 
        { _expr=(Func {| Name=fn.Name; Body=body'; Parameters=fn.Parameters; TyAnnot=fn.TyAnnot |}); Loc=expr.Loc; Metadata=expr.Metadata }
    | Var v ->
        match v.TyAnnot with
        | Some ty ->
            match v.InitExprOpt with
            | Some initExpr ->
                match initExpr._expr with
                | AnonFunc anFn ->
                    let body' = transformFunction anFn.Body 
                    { _expr=(Func {| Name=v.Name; Body=body'; Parameters=anFn.Parameters; TyAnnot=ty |}); Loc=expr.Loc; Metadata=anFn.TyAnnot }
                | _ -> expr
            | None -> expr
        | None ->
            match v.InitExprOpt with
            | Some initExpr ->
                match initExpr._expr with
                | AnonFunc anFn ->
                    let body' = transformFunction anFn.Body 
                    { _expr=(Func {| Name=v.Name; Body=body'; Parameters=anFn.Parameters; TyAnnot=anFn.TyAnnot |}); Loc=expr.Loc; Metadata=anFn.TyAnnot }
                | _ -> expr
            | None -> expr
    | Entrypoint fnExpr ->
        let fnExpr' = transformFunction fnExpr
        { _expr=(Entrypoint fnExpr'); Loc=expr.Loc; Metadata=expr.Metadata }
    | _ -> expr

let transformFunctions exprs =
    List.map transformFunction exprs

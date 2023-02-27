module Koltrast.Frontend.FuncTransform

open Koltrast.Frontend.AST

let rec transformFunctions expr =
    match expr._expr with
    | Var v ->
        match v.InitExprOpt with
        | Some initExpr ->
            match initExpr._expr with
            | AnonFunc fn ->
                { _expr=(Func {| Name=v.Name; Parameters=fn.Parameters; Body=fn.Body; TyAnnot=fn.TyAnnot |}); Loc=expr.Loc; Metadata=() }
            | _ -> expr
        | None -> expr
    | Block bl ->
        let bl' = List.map (fun a -> transformFunctions a) bl
        { _expr=(Block bl'); Loc=expr.Loc; Metadata=() }
    | _ -> expr
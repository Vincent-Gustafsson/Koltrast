module Koltrast.Frontend.ValidateTopLevel

open Koltrast.Diagnostics
open Koltrast.Frontend.AST

let validateTopLevel (diagnostics: DiagnosticBag) exprs: Result<UntypedExpr list,DiagnosticBag> =
    exprs
    |> List.map (fun expr ->
        match expr._expr with
        | Func _
        | Var _
        | Entrypoint _ -> Some expr
        | _ ->
            diagnostics.add {
                Message="only functions and variables are allowed at the top-level"
                Hint=""
                Level=DiagnosticLevel.Error
                Kind=DiagnosticKind.Other
                Loc=expr.Loc
            }; None
        )
    |> (fun result ->
    if List.exists Option.isNone result then
        Result.Error diagnostics
    else
        result
        |> List.map (function | Some expr -> expr)
        |> Ok)

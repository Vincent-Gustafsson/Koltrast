module Koltrast.Frontend.ValidateEntrypoint

open Koltrast.Location
open Koltrast.Diagnostics
open Koltrast.Frontend.AST

let isEntrypoint expr =
    match expr._expr with
    | Entrypoint _ -> true
    | _ -> false

let getEntrypointName expr =
    match expr._expr with
    | Entrypoint expr ->
        match expr._expr with
        | Func fn -> fn.Name

let partitionResults (results: Result<'T, 'TError> list) =
    // I have to reverse the list if I want to preserve the original order
    // because of tail recursion.
    let revResults = List.rev results
    let rec loop oks errors = function
        | [] -> (oks, errors)
        | Ok x :: xs -> loop (x :: oks) errors xs
        | Result.Error e :: xs -> loop oks (e :: errors) xs
    loop [] [] revResults

let validateEntrypoint (diagnostics: DiagnosticBag) compUnit: Result<UntypedExpr list,DiagnosticBag> =
    let reportTooManyEntrypointsError firstEntrypoint expr =
        { Message = ($"an entrypoint already exists '{firstEntrypoint}'")
          Hint = ""
          Loc = expr.Loc
          Level = DiagnosticLevel.Error
          Kind = DiagnosticKind.Program }

    let result, _, numOfEntrypoints =
        (([], "", 0), compUnit)
        ||> List.fold (fun (result, firstEntrypoint, numOfEntrypoints) expr ->
            if isEntrypoint expr then
                if numOfEntrypoints > 0 then
                    result @ [Result.Error(reportTooManyEntrypointsError firstEntrypoint expr)],
                    firstEntrypoint, numOfEntrypoints + 1
                else
                    result @ [Ok expr], getEntrypointName expr, numOfEntrypoints + 1
            else
                result @ [Ok expr], firstEntrypoint, numOfEntrypoints
            )
        
    let exprs, errors = partitionResults result
    
    if numOfEntrypoints < 1 then
        diagnostics.add {
          Message = ($"program must have an entrypoint")
          Hint = ""
          Loc = diagnostics.BEGINNING_OF_FILE
          Level = DiagnosticLevel.Error
          Kind = DiagnosticKind.Program }
        Result.Error diagnostics
    else
        if errors.Length = 0 then
            Ok exprs
        else
            List.iter diagnostics.add errors
            Result.Error diagnostics

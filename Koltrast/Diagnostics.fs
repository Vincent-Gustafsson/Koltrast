module Koltrast.Diagnostics

open System.IO
open Koltrast.Location

type DiagnosticLevel =
    | Info
    | Warning
    | Error
    override x.ToString() =
        match x with
        | Info -> "INFO"
        | Warning -> "WARNING"
        | Error -> "ERROR"

type DiagnosticKind =
    | Type
    override x.ToString() =
        match x with
        | Type -> "TYPE"

type Diagnostic = {
    Message: string
    Hint: string
    Loc: Location
    Level: DiagnosticLevel
    Kind: DiagnosticKind
}

type DiagnosticBag(_sourcePath) =
    let sourcePath: string = _sourcePath
    let source: string list = File.ReadAllLines sourcePath |> List.ofArray
    let mutable diagnostics: Diagnostic list = []
    member this.add (d: Diagnostic) = diagnostics <- diagnostics @ [d]
    member this.genDiagnostics() =
        diagnostics |> List.map (fun d ->
            let loc = d.Loc
            // let ctxRange = [List.fr(loc.Start.Line-1)..(loc.End.Line-1)]
            // let ctxLines =
            //    source
            //    |> List.mapi (fun i line -> if List.contains i ctxRange then [(i+1, line)] else [])
            //    |> List.concat
            let ctxLine = source[loc.Start.Line-1]
            let errKindStr = $"[{d.Kind} {d.Level}]"
            let locStr = $"{sourcePath}:{loc.Start.Line}:{loc.Start.Col}"
            let hintStr = ((String.replicate (loc.End.Index - loc.Start.Index) "^") + $" {d.Hint}")
            let paddedHintStr = hintStr.PadLeft(int loc.Start.Col + hintStr.Length, '_')
            
            $"
{errKindStr}: {d.Message}
--> {locStr}
 |
 | {ctxLine}
 |{paddedHintStr}"
        )

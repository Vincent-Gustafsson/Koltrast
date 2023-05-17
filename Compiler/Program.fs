module Compiler.Program

open Compiler.Parse
open FParsec
open Parser

let prog = """"""

[<EntryPoint>]
let main args =
    match runParserOnString pProgram () "internal" prog with
    | Success(res, _, _) -> printfn "%A" res
    | Failure(s, _, _) -> printfn $"{s}"
    0

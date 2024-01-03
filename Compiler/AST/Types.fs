module Compiler.AST.Types

type Type =
    | I8
    | I32
    | Bool
    | Unit
    | Fn of FnType
    with
        override x.ToString() =
            match x with
            | I8 -> "i8"
            | I32 -> "i32"
            | Bool -> "bool"
            | Unit -> "unit"
            | Fn fnTy -> $"""({String.concat " -> " (List.map (fun x -> x.ToString()) fnTy.Parameters) } -> {fnTy.Return})"""

and FnType = {| Parameters: Type list; Return: Type |}

let strToType str =
    match str with
    | "i8" -> Some I8
    | "i32" -> Some I32
    | "unit" -> Some Unit
    | "bool" -> Some Bool
    | _ -> None

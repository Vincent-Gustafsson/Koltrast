module Compiler.AST.Types

type Type =
    | I8
    | I32
    | Bool
    | Unit
    | Fn of {| Parameters: Type list; Return: Type |}
    with
        override x.ToString() =
            match x with
            | I8 -> "i8"
            | I32 -> "i32"
            | Bool -> "i8"
            | Unit -> "unit"
            | Fn fnTy -> $"""({String.concat " -> " (List.map (fun x -> x.ToString()) fnTy.Parameters) } -> {fnTy.Return})"""
            

module Koltrast.Frontend.AST

type Location = {
    StreamName: string
    Start: {| Index: int; Line: int; Col: int |}
    End: {| Index: int; Line: int; Col: int  |}
}

type BinOpKind =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | EQ
    | GT
    | LT

let binOpStr op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | EQ -> "=="
    | GT -> ">"
    | LT -> "<"

type Mutability =
    | Mutable
    | Immutable

type Type =
    | Int
    | Bool
    | Unit
//    | UserDefined of string

let strToType str =
    match str with
    | "int" -> Some Int
    | "unit" -> Some Unit
    | "bool" -> Some Bool
    | _ -> None

type Expr<'a> =
    | NumericLiteral of 'a * Location * int
    | BoolLiteral of 'a * Location * bool
    | Ident of 'a * Location * string
    | BinOp of 'a * Location * BinOpKind * Expr<'a> * Expr<'a>
    | Assign of 'a * Location * Expr<'a> * Expr<'a>
    | AnnVarDecl of 'a * Location * string * Mutability * Type * Option<Expr<'a>>
    | InferredVarDecl of 'a * Location * string * Mutability * Option<Type> * Expr<'a>
    | If of 'a * Location * Expr<'a> * Expr<'a> * Expr<'a>
    | Block of 'a * Location * Expr<'a> list
    | Func of 'a * Location * string * (string * Type) list * Type * Expr<'a>
    | Call of 'a * Location * string * Expr<'a> list
    | While of 'a * Location * Expr<'a> * Expr<'a>
    // For prototyping
    | Print of 'a * Location * Expr<'a>

type UntypedExpr = Expr<unit>
type TypedExpr = Expr<Type>

type CompilationUnit<'a> = | CompilationUnit of Expr<'a> 

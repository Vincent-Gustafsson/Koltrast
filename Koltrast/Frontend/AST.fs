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

let binOpStr op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"

type Mutability =
    | Mutable
    | Immutable

type Type =
    | Int
    | Bool
    | UserDefined of string
    | Void

let strToType str =
    match str with
    | "int" -> Some Int
    | "void" -> Some Void
    | _ -> Some (UserDefined str)

type Expr<'a> =
    | NumericLiteral of 'a * Location * int
    | BoolLiteral of 'a * Location * bool
    | Ident of 'a * Location * string
    | BinOp of 'a * Location * BinOpKind * Expr<'a> * Expr<'a>
    | Assign of 'a * Location * Expr<'a> * Expr<'a>

type UntypedExpr = Expr<unit>
type TypedExpr = Expr<Type>

type Stmt<'a> = 
    | AnnVarDecl of 'a * Location * string * Mutability * Type * Option<Expr<'a>>
    | InferredVarDecl of 'a * Location * string * Mutability * Option<Type> * Expr<'a>
    | Block of 'a * Location * Stmt<'a> list
    | ExprStmt of 'a * Location * Expr<'a>

type UntypedStmt = Stmt<unit>
type TypedStmt = Stmt<Type>

type CompilationUnit<'a> = ('a * Stmt<'a>) 

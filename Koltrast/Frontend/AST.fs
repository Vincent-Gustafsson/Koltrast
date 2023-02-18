module Koltrast.Frontend.AST

type Location = {
    StreamName: string
    Start: {| Index: int; Line: int; Col: int |}
    End: {| Index: int; Line: int; Col: int  |}
}

type Type =
    | I8
    | I64
    | Bool
    | Unit
    | Array of Type * int
    | Fun of Type list * Type

let rec typeToStr = function
    | I64 -> "i32"
    | Bool -> "bool"
    | Unit -> "unit"
    | Array(ty, size) -> $"[{typeToStr ty} ; {size}]"
    | Fun(parameters, retTy) -> $"""({String.concat " -> " (List.map typeToStr parameters) } -> {typeToStr retTy})"""

let strToType str =
    match str with
    | "int" -> Some I64
    | "unit" -> Some Unit
    | "bool" -> Some Bool
    | _ -> None

type BinOpKind =
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Eq
    | GtEq
    | LtEq
    | Gt
    | Lt

let binOpStr op =
    match op with
    | Add -> "+"
    | Sub -> "-"
    | Mul -> "*"
    | Div -> "/"
    | Mod -> "%"
    | Eq -> "=="
    | GtEq -> ">="
    | LtEq -> "<="
    | Gt -> ">"
    | Lt -> "<"

type Mutability =
    | Mutable
    | Immutable

type ExprKind<'a> =
    | Annot of {| AnnotatedExpr: Expr<'a>; Ty: Type |}
    | NumericLiteral of int
    | BoolLiteral of bool
    | Ident of IdentExprKind
    | Block of BlockExprKind<'a>
    | BinOp of {| Op: BinOpKind;  Left: Expr<'a>; Right: Expr<'a> |}
    | Assign of {| Name: IdentExprKind; AssignExpr: Expr<'a> |}
    | VarDecl of {| Name: IdentExprKind; Mut: Mutability; InitExprOpt: Option<Expr<'a>> |}
    | Func of {| Name: IdentExprKind;  Parameters: IdentExprKind list; Body: BlockExprKind<'a> |}
    | If of {| Cond: Expr<'a>; ThenExpr: Expr<'a>; ElseExpr: Expr<'a> |}
    | FuncAppl of {| Name: IdentExprKind; Arguments: FuncArguments<'a> |}
    | While of {| Cond: Expr<'a>; Body: BlockExprKind<'a> |}
    | Print of Expr<'a> // For prototyping

and BlockExprKind<'a> = Expr<'a> list
and IdentExprKind = string
and FuncArguments<'a> = Expr<'a> list

and Expr<'a> = {
    _expr: ExprKind<'a>
    Loc: Location
    Metadata: 'a
}

type UntypedExpr = Expr<unit>
type TypedExpr = Expr<Type>

type UntypedCompilationUnit = UntypedExpr
type TypedCompilationUnit = TypedExpr

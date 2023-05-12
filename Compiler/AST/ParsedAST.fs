module Compiler.AST.ParsedAST

open Types

type Location = {
    StreamName: string
    Start: {| Index: int; Line: int; Col: int |}
    End: {| Index: int; Line: int; Col: int  |}
}

type Identifier = string

type BinOpKind =
    | Add
    | Sub
    | Mul
    | Div

type ExprKind =
    | Error of string
    
    | Block of Expr list
    | Ident of Identifier
    | IntegerLiteral of int
    | BooleanLiteral of bool
    | BinOp of {| Op: BinOpKind; Left: Expr; Right: Expr |}
    | IfExpr of {| Cond: Expr; Then: Expr; Else: Expr |}
    | FuncAppl of {| Name: Identifier; Arguments: Expr |}
    | While of {| Cond: Expr; Body: Expr |}
    | LetVar of {| Name: Expr; InitExpr: Expr |}
    | ConstVar of {| Name: Expr; InitExpr: Expr |}
    | Assign of {| Name: Expr; AssExpr: Expr |}

and Expr = {
    _expr: ExprKind
    Loc: Location
}
type FnParam = {| Name: Identifier; Ty: Type |}

type Item =
    | Function of {| Name: Identifier; Params: FnParam list; |}

type CompilationUnit = {
    Items: Item
}

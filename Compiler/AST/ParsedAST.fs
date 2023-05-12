module Compiler.AST.AST

open Types

type Identifier = string

type BinOpKind =
    | Add
    | Sub
    | Mul
    | Div

type Expr =
    | Block of Expr list
    | IntegerLiteral of int
    | BooleanLiteral of bool
    | BinOp of {| Op: BinOpKind; Left: Expr; Right: Expr |}
    | IfExpr of {| Cond: Expr; Then: Expr; Else: Expr |}
    | FuncAppl of {| Name: Identifier; Arguments: Expr |}
    | While of {| Cond: Expr; Body: Expr |}
    | LetVar of {| Name: Identifier; InitExpr: Expr |}
    | ConstVar of {| Name: Identifier; InitExpr: Expr |}
    | Assign of {| Name: Identifier; AssExpr: Expr |}

type FnParam = {| Name: Identifier; Ty: Type |}

type Item =
    | Function of {| Name: Identifier; Params: FnParam list; |}

type CompilationUnit = {
    Items: Item
}

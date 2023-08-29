module Compiler.AST.TypedAST

open Compiler.AST.Types
open Compiler.AST.ParsedAST

type TExprKind =
    | Error of string
    
    | Block of TExpr list
    | Ident of Identifier
    | IntegerLiteral of int
    | BooleanLiteral of bool
    | BinOp of {| Op: BinOpKind; Left: TExpr; Right: TExpr |}
    | IfExpr of {| Cond: TExpr; Then: TExpr; Else: TExpr |}
    | FuncAppl of {| Name: TExpr; Arguments: TExpr list |}
    | While of {| Cond: TExpr; Body: TExpr |}
    | LetVar of {| Name: Expr; InitExpr: TExpr |}
    | ConstVar of {| Name: Expr; InitExpr: TExpr |}
    | Assign of {| Name: Expr; AssExpr: TExpr |}

and TExpr = {
    _expr: TExprKind
    Ty: Type
    Loc: Location
}

type TItem = {
    _item: ItemKind
    Ty: Type
    Loc: Location
}

type TCompilationUnit = {
    Items: TItem list
}


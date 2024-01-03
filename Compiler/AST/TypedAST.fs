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
with
    override x.ToString() =
        match x with
        | Error _ -> failwith "unreachable"
        | Block tExprs -> $"""{{{tExprs |> List.map (fun e -> e.ToString()) |> (String.concat "\n")}}}"""
        | Ident name -> name
        | IntegerLiteral i -> "int"
        | BooleanLiteral b -> "bool"
        | BinOp b -> $"({b.Left.Ty} {b.Op} {b.Right.Ty})"
        | IfExpr iff -> $"if {iff.Cond.Ty} then {iff.Then.Ty} else {iff.Else.Ty}"
        | FuncAppl foo -> failwith "todo"
        | While foo -> $"while {foo.Cond.Ty} \n{foo.Body}"
        | LetVar v -> $"let ident = {v.InitExpr}"
        | ConstVar v -> $"const ident = {v.InitExpr}"
        | Assign foo -> $"ident = {foo.AssExpr}"

and TExpr = {
    _expr: TExprKind
    Ty: Type
    Loc: Location
}
with
    override x.ToString() = x._expr.ToString()

type TItemKind =
    | Function of {| Name: Expr; Params: FnParam list; ReturnType: Type; Body: TExpr |}
with
    override x.ToString() =
        match x with
        | Function foo -> $"fn ident() -> {foo.Body}"
type TItem = {
    _item: TItemKind
    Ty: Type
    Loc: Location
}
with
    override x.ToString() = x._item.ToString()
type TCompilationUnit = {
    Items: TItem list
}
with
    override x.ToString() = x.Items |> List.map (fun e -> e.ToString()) |> (String.concat "\n")
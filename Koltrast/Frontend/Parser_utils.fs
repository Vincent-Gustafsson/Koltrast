module Koltrast.Frontend.Parser_utils

open FParsec
open AST

let getNameFromIdent (expr: Expr<'a>) =
    match expr with
    | Ident(_,_,name) -> name

let getExprLoc (expr: UntypedExpr) =
    match expr with
    | NumericLiteral(_,l,_) -> l
    | BoolLiteral(_,l,_) -> l
    | Ident(_, l, _) -> l
    | BinOp(_,l,_,_,_) -> l
    | Assign(_,l,_,_) -> l
    | If(_,l,_,_,_) -> l
    | Block(_,l,_) -> l
    | Func(_,l,_,_,_,_) -> l
    | Call(_,l,_,_) -> l

let isValidIdentChar c =
    ['_'] @ ['A'..'Z'] @ ['a'..'z']
    |> Seq.exists (fun ch -> ch = c)

let isKeyword str =
    [| "let"; "mut" |]
    |> Seq.exists (fun kw -> str = kw)

let locFromFParsecPos (startPos: Position) (endPos: Position) = {
    StreamName = startPos.StreamName
    Start = {| Index=int startPos.Index; Line=int startPos.Line; Col=int startPos.Column |}
    End = {| Index=int endPos.Index; Line=int endPos.Line; Col=int endPos.Column |}
}

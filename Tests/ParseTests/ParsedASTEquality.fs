module Tests.ParseTests.ParsedASTEquality

open System
open System.Collections
open System.Collections.Generic
open Compiler.AST.ParsedAST
open Compiler.AST.Types

// Couldn't use List.forall2 since it throws an exception if the lists have different lengths
// and I want that to be one of the conditions of the equality check.
/// Compares the two lists elements and checks for equality. If the lists are of different lengths the lists aren't considered equal.
let listsEq f l1 l2 =
    List.length l1 = List.length l2
    && List.forall2 f l1 l2
    
let rec eqExpr expr1 expr2 =
    match expr1._expr, expr2._expr with
    | Error err1, Error err2 -> err1 = err2
    | Block exprs1, Block exprs2 -> listsEq eqExpr exprs1 exprs2
    | Ident id1, Ident id2 -> id1 = id2
    | IntegerLiteral n1, IntegerLiteral n2 -> n1 = n2
    | BooleanLiteral b1, BooleanLiteral b2 -> b1 = b2
    | BinOp bin1, BinOp bin2 ->
        bin1.Op = bin2.Op
        && eqExpr bin1.Left bin2.Left
        && eqExpr bin1.Right bin2.Right
    | IfExpr if1, IfExpr if2 ->
        eqExpr if1.Cond if2.Cond
        && eqExpr if1.Then if2.Then
        && eqExpr if1.Else if2.Else
    | FuncAppl appl1, FuncAppl appl2 ->
        eqExpr appl1.Name appl2.Name
        && listsEq eqExpr appl1.Arguments appl2.Arguments
    | While w1, While w2 ->
        eqExpr w1.Cond w2.Cond
        && eqExpr w1.Body w2.Body
    | LetVar v1, LetVar v2
    | ConstVar v1, ConstVar v2 ->
        eqExpr v1.Name v2.Name
        && eqExpr v1.InitExpr v2.InitExpr
    | Assign ass1, Assign ass2 ->
        eqExpr ass1.Name ass2.Name
        && eqExpr ass1.AssExpr ass2.AssExpr
    | _ -> false

let eqFnParam (p1: FnParam) (p2: FnParam) =
    eqExpr p1.Name p2.Name && p1.Ty = p2.Ty

let eqItem item1 item2 =
    match item1._item, item2._item with
    | Function fn1, Function fn2 ->
        eqExpr fn1.Name fn2.Name
        && listsEq eqFnParam fn1.Params fn2.Params
        && fn1.ReturnType = fn2.ReturnType
        && eqExpr fn1.Body fn2.Body
    | _ -> false

let eqProg prog1 prog2 =
    listsEq eqItem prog1.Items prog2.Items

type ProgramEqualityComparer() =
    interface IEqualityComparer<CompilationUnit> with
        member this.Equals(p1, p2) =
            eqProg p1 p2
        member this.GetHashCode(_) = raise(NotImplementedException())

type ItemEqualityComparer() =
    interface IEqualityComparer<Item> with
        member this.Equals(item1, item2) =
            eqItem item1 item2
        member this.GetHashCode(_) = raise(NotImplementedException())

type ExprEqualityComparer() =
    interface IEqualityComparer<Expr> with
        member this.Equals(expr1, expr2) =
            eqExpr expr1 expr2
        member this.GetHashCode(_) = raise(NotImplementedException())


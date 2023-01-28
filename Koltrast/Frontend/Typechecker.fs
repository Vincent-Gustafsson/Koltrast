module Koltrast.Frontend.Typechecker

open System.IO
open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Environment

let mkTypeError (src: string list) (loc: Location) (errMsg: string) (hint: string) =
    let hintLineChar = "-"
    $"
--> {loc.StreamName}:{loc.Start.Line}:{loc.Start.Col} {errMsg}
 | {src[loc.Start.Line-1]}
 | {String.replicate (loc.End.Index - loc.Start.Index) hintLineChar } {hint}
    "

let rec infer (src: string list) (env: Env) (node: UntypedNode): Result<(TypedNode * Type * Env), string> =
    let currScope = env.Peek()
    match node with
    | NumericLiteral(_,loc, num) -> Ok(TypedNode.NumericLiteral(Type.Int, loc, num), Type.Int, env)
    | BoolLiteral(_,loc, b) -> Ok(TypedNode.BoolLiteral(Type.Bool, loc, b), Type.Bool, env)
    | BinOp(_,loc, op, l, r) ->
        result {
            let! (typedL, lType) = infer src env l
            let! (typedR, rType) = infer src env r
            
            let! res =
                if lType <> rType then
                    Error(
                        mkTypeError
                           src
                           loc
                           $"operands' types not consistent. they have type {lType} and {rType}"
                           $"{lType} {binOpStr op} {rType}"
                    )
                else
                    match op with
                    | Add | Sub | Mul | Div ->
                        if lType = Type.Int then
                            Ok (TypedNode.BinOp (Int, loc, op, typedL, typedR), Int, env)
                        else
                            Error(
                                mkTypeError
                                    src
                                    loc
                                    $"invalid operand types. they have type {lType} and {rType}, expected ({Int})"
                                    $"{lType} {binOpStr op} {rType}"
                            )
            return res
        }
    | VarDecl(_,loc, name, mutability, tyAnnoOpt, initExprOpt) ->
        let tyAnnotation =
            match tyAnnoOpt with
            | Some tyStr -> strToType tyStr
            | None -> None
        
        let tInitExpr =
            match initExprOpt with
            | Some initExpr ->
                match infer src env initExpr with
                | Ok(tInitExpr, initExprTy, env') ->
                    
        
        let res = Scope.addVar currScope name (VarSymbol(tyAnnotation, mutability))
        match res with
        | Ok scope' ->
            env.Pop() |> ignore
            env.Push(scope')
            Ok(TypedNode.VarDecl(Void, loc, name, mutability, tyAnnoOpt, tInitExpr), Void, env)
        | Error err -> Error(mkTypeError err)
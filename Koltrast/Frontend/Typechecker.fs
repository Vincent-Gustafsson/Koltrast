module Koltrast.Frontend.Typechecker

open System.IO
open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser_utils
open Microsoft.FSharp.Collections

type Env = Map<string, (Type * Mutability)>

module Env =
    let empty = Map.empty
    let addVar env key value = Map.add key value env
    let lookup env key: Option<Type * Mutability> = Map.tryFind key env

let mkTypeError (src: string list) (loc: Location) (errMsg: string) (hint: string) =
    let line = System.IO.File.ReadAllLines(loc.StreamName)[loc.Start.Line - 1 ]
    let coolStr = (String.replicate (loc.End.Col - loc.Start.Col) "^") + $" {hint}"
    let last n xs = Array.toSeq xs |> Seq.skip (xs.Length - n) |> Seq.toList
    let filePath = 
        loc.StreamName.Split('\\')
        |> last 2
        |> String.concat "\\"

    let final = $"
[TYPE ERROR]: {errMsg}
--> {filePath}:{loc.Start.Line}:{loc.Start.Col}
 |
 | {line}
 |{coolStr.PadLeft(int loc.Start.Col + coolStr.Length, '_')}"

    final

let rec inferExpr (src: string list) (env: Env) (expr: UntypedExpr): Result<TypedExpr * Type, string list> =
    let infer = inferExpr src
    let check = checkExpr src
    
    match expr with
    | NumericLiteral(_,loc, num) -> Ok(TypedExpr.NumericLiteral(Type.Int, loc, num), Type.Int)
    | BoolLiteral(_,loc, b) -> Ok(TypedExpr.BoolLiteral(Type.Bool, loc, b), Type.Bool)
    | Ident(_,loc, name) ->
        match Env.lookup env name with
        | Some (ty, _) -> Ok(TypedExpr.Ident(ty, loc, name), ty)
        | None ->
            Error([
                mkTypeError
                    src
                    loc
                    $"undefined variable {name}"
                    $""
            ])
    | BinOp(_,loc, op, l, r) ->
        result {
            let! (typedL, lType) = infer env l
            let! (typedR, rType) = infer env r
            
            let! res =
                if lType <> rType then
                    Error([
                        mkTypeError
                           src
                           loc
                           $"operands' types not consistent. they have type {lType} and {rType}"
                           $"{lType} {binOpStr op} {rType}"
                    ])
                else
                    match op with
                    | Add | Sub | Mul | Div ->
                        if lType = Type.Int then
                            Ok (TypedExpr.BinOp (Int, loc, op, typedL, typedR), Int)
                        else
                            Error([
                                mkTypeError
                                    src
                                    loc
                                    $"invalid operand types. they have type {lType} and {rType}, expected ({Int})"
                                    $"{lType} {binOpStr op} {rType}"
                            ])
            return res
        }
    | Assign(_, loc, l, r) ->
        match l with
        | Ident(_, loc, name) ->
            match infer env l with
            | Ok(typedIdent, identTy) ->
                match Env.lookup env name with
                | Some(_, Mutable) ->
                    match check env r identTy with
                    | Ok typedR -> Ok(TypedExpr.Assign(identTy, loc, typedIdent, typedR), identTy)
                    | Error e -> Error e
                        (**Error([
                            mkTypeError
                                src
                                (getExprLoc r)
                                $"variable {name} is an {identTy}"
                                $"expected {identTy}"
                        ])**)
                | _ ->
                    Error([
                        mkTypeError
                            src
                            loc
                            $"trying to assign to an immutable variable"
                            $"{name} is immutable"
                    ])
            | Error e -> Error e
        | _ ->
            Error([
                mkTypeError
                    src
                    (getExprLoc l)
                    $"Expected an identifier"
                    $""
            ])
    | _ -> Error(["What the fuck?"])

and checkExpr (src: string list) (env: Env) (expr: UntypedExpr) (expectedTy: Type): Result<TypedExpr, string list> =
    let infer = inferExpr src

    match expr with
    | _ ->
        match infer env expr with
        | Ok(typedExpr, exprTy) ->
            let loc = getExprLoc expr
            if exprTy = expectedTy then
                Ok typedExpr
            else
                Error([
                    mkTypeError
                        src
                        loc
                        $"type mismatch, {expectedTy} and {exprTy}"
                        $"expected {expectedTy}"
                ])
        | Error e -> Error e

let rec checkStmt (src: string list) (env: Env) (stmt: UntypedStmt): Result<Env * TypedStmt, string list> =
    let check = checkExpr src
    let infer = inferExpr src
    
    match stmt with
    | AnnVarDecl(_, loc, name, mut, ty, initExprOpt) ->
        match initExprOpt with
        | Some initExpr ->
            match check env initExpr ty with
            | Ok typedInitExpr ->
                let env' = Env.addVar env name (ty, mut)
                Ok(env', TypedStmt.AnnVarDecl(Void, loc, name, mut, ty, Some typedInitExpr))
            | Error e -> Error e
        | None ->
            let env' = Env.addVar env name (ty, mut)
            Ok(env', TypedStmt.AnnVarDecl(Void, loc, name, mut, ty, None))
    | InferredVarDecl(_, loc, name, mut, tyOpt, expr) ->
        match tyOpt with
        | Some ty ->
            match check env expr ty with
            | Ok typedInitExpr ->
                let env' = Env.addVar env name (ty, mut)
                Ok(env', TypedStmt.InferredVarDecl(Void, loc, name, mut, Some ty, typedInitExpr))
            | Error e -> Error e
        | None ->
            match infer env expr with
            | Ok(typedInitExpr, initExprTy) ->
                let env' = Env.addVar env name (initExprTy, mut)
                Ok(env', TypedStmt.InferredVarDecl(Void, loc, name, mut, Some initExprTy, typedInitExpr))
            | Error e -> Error e
    | Block(_, loc, stmts) ->
        ((env, []), stmts)
        ||> List.mapFold (fun (env, errors) stmt ->
            match checkStmt src env stmt with
            | Ok(env', tyStmt) -> [tyStmt], (env', errors)
            | Error e -> [], (env, errors @ e)
        )
        |> (fun (checkedStmts, (env', errors)) ->
            let typedStmts = List.concat checkedStmts
            if errors.IsEmpty then
                Ok(env, TypedStmt.Block(Void, loc, typedStmts))
            else
                Error errors
        )
    | ExprStmt(_, loc, expr) ->
        match infer env expr with
        | Ok (typedExpr, _) -> Ok(env, TypedStmt.ExprStmt(Void, loc, typedExpr))
        | Error e -> Error e
        
let checkCompilationUnit (src: string list) (compUnit: CompilationUnit<unit>): Result<CompilationUnit<Type>, string list> =
    let env = Env.empty
    match checkStmt src env (compUnit |> snd) with
    | Ok (_, typedStmt) -> Ok(CompilationUnit(Void, typedStmt))
    | Error e -> Error e

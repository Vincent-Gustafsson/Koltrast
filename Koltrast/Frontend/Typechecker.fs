module Koltrast.Frontend.Typechecker

open System.IO
open Koltrast.ResultBuilder
open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser_utils
open Microsoft.FSharp.Collections

type Env = {
    vars: Map<string, (Type * Mutability)>
    funcs: Map<string, Type>
}

module Env =
    let empty = {vars=Map.empty; funcs=Map.empty}
    let addVar env key value = {env with vars=(Map.add key value env.vars)}
    let lookupVar env key: Option<Type * Mutability> = Map.tryFind key (env.vars)
    let addFunc env key value = {env with funcs=(Map.add key value env.funcs)}
    let lookupFunc env key: Option<Type> = Map.tryFind key (env.funcs)


let mkTypeError (src: string list) (loc: Location) (errMsg: string) (hint: string) =
    let line = System.IO.File.ReadAllLines(loc.StreamName)[loc.Start.Line - 1 ]
    let coolStr = (String.replicate (loc.End.Index - loc.Start.Index) "^") + $" {hint}"
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
    
let rec typeBlock (src: string list) (env: Env) (loc: Location) (exprs: UntypedExpr list) =
    let infer = inferExpr src
    
    (env, exprs)
    ||>
    List.mapFold (fun env expr ->
        match infer env expr with
        | Ok(tExpr, ty) ->
            match ty with
            | Type.Fun _ ->
                match tExpr with
                | InferredVarDecl(_, _, name, mut, tyOpt, _) ->
                    let env' = Env.addFunc env name ty
                    (Ok(tExpr, ty), env')
                | AnnVarDecl(_, _, name, mut, ty, _) ->
                    let env' = Env.addFunc env name ty
                    (Ok(tExpr, ty), env')
                | Func(ty, _, name, _, _, _) ->
                    let env' = Env.addFunc env name ty
                    (Ok(tExpr, ty), env')
                | _ ->
                    (Ok(tExpr, ty), env)
                
            | _ ->
                match tExpr with
                | InferredVarDecl(_, _, name, mut, tyOpt, _) ->
                    let env' = Env.addVar env name (ty, mut)
                    (Ok(tExpr, ty), env')
                | AnnVarDecl(_, _, name, mut, ty, _) ->
                    let env' = Env.addVar env name (ty, mut)
                    (Ok(tExpr, ty), env')
                | Func(ty, _, name, _, _, _) ->
                    let env' = Env.addFunc env name ty
                    (Ok(tExpr, ty), env')
                | _ ->
                    (Ok(tExpr, ty), env)
        | Error e -> (Error e, env)
    )
    |> (fun (checkResults, env') ->
        let ok, errors =
            (([], []), checkResults)
            ||> List.fold (fun (ok, errs) r ->
                match r with
                | Ok (tExpr, ty) -> ok @ [(tExpr, ty)], errs
                | Error e -> ok, errs @ e)
        
        match ok, errors with
        | [], [] -> Ok(TypedExpr.Block(Type.Unit, loc, []), Type.Unit)
        | [(tExpr, ty)], [] ->
            Ok(TypedExpr.Block(ty, loc, [tExpr]), ty)
        | ok, [] ->
            let tExprs, types = ok |> List.unzip
            let lastTy = types |> List.last
            Ok(TypedExpr.Block(lastTy, loc, tExprs), lastTy)
        | _, e -> Error e
    )

and inferExpr (src: string list) (env: Env) (expr: UntypedExpr): Result<TypedExpr * Type, string list> =
    let infer = inferExpr src
    let check = checkExpr src
    
    match expr with
    | NumericLiteral(_,loc, num) -> Ok(TypedExpr.NumericLiteral(Type.I64, loc, num), Type.I64)
    | BoolLiteral(_,loc, b) -> Ok(TypedExpr.BoolLiteral(Type.Bool, loc, b), Type.Bool)
    | Ident(_,loc, name) ->
        match Env.lookupVar env name, Env.lookupFunc env name with
        | Some (ty, _), None | None, Some (ty)  -> Ok(TypedExpr.Ident(ty, loc, name), ty)
        | None, None ->
            Error([
                mkTypeError
                    src
                    loc
                    $"undefined name {name}"
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
                           $"operands' types not consistent. they have type {typeToStr lType} and {typeToStr rType}"
                           $"{typeToStr lType} {binOpStr op} {typeToStr rType}"
                    ])
                else
                    match op with
                    | Add | Sub | Mul | Div | Mod ->
                        if lType = Type.I64 then
                            Ok (TypedExpr.BinOp (I64, loc, op, typedL, typedR), I64)
                        else
                            Error([
                                mkTypeError
                                    src
                                    loc
                                    $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I64})"
                                    $"{typeToStr lType} {binOpStr op} {typeToStr rType}"
                            ])
                    | GtEq | LtEq | Gt | Lt ->
                        match lType, rType with
                        | I64, I64 -> Ok(TypedExpr.BinOp(Bool, loc, op, typedL, typedR), Bool)
                        | _ -> Error([
                                mkTypeError
                                    src
                                    loc
                                    $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I64})"
                                    $"{typeToStr lType} {binOpStr op} {typeToStr rType}"
                            ])
                    | Eq ->
                        if lType = rType then
                            Ok (TypedExpr.BinOp (Bool, loc, op, typedL, typedR), Bool)
                        else
                            Error([
                                mkTypeError
                                    src
                                    loc
                                    $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I64} or {typeToStr Bool}). ALSO, MISMATCH MAYBE TOO LAZY TO FIX GOOD ERROR REPORTING :)"
                                    $"{typeToStr lType} {binOpStr op} {typeToStr rType}"
                            ])

            return res
        }
    | Assign(_, loc, l, r) ->
        match l with
        | Ident(_, loc, name) ->
            match infer env l with
            | Ok(typedIdent, identTy) ->
                match Env.lookupVar env name with
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
    | AnnVarDecl(_, loc, name, mut, ty, initExprOpt) ->
        match initExprOpt with
        | Some initExpr ->
            match check env initExpr ty with
            | Ok typedInitExpr ->
                let env' = Env.addVar env name (ty, mut)
                Ok(TypedExpr.AnnVarDecl(Type.Unit, loc, name, mut, ty, Some typedInitExpr), Type.Unit)
            | Error e -> Error e
        | None ->
            let env' = Env.addVar env name (ty, mut)
            Ok(TypedExpr.AnnVarDecl(ty, loc, name, mut, ty, None), ty)
    | InferredVarDecl(_, loc, name, mut, tyOpt, expr) ->
        match tyOpt with
        | Some ty ->
            match check env expr ty with
            | Ok typedInitExpr ->
                Ok(TypedExpr.InferredVarDecl(ty, loc, name, mut, Some ty, typedInitExpr), ty)
            | Error e -> Error e
        | None ->
            match infer env expr with
            | Ok(typedInitExpr, initExprTy) ->
                
                
                let env' =
                    match initExprTy with
                    | Type.Fun _ -> Env.addFunc env name initExprTy
                    | _ -> Env.addVar env name (initExprTy, mut)
                
                
                
                // let env' = Env.addVar env name (initExprTy, mut)
                
                
                
                Ok(TypedExpr.InferredVarDecl(initExprTy, loc, name, mut, Some initExprTy, typedInitExpr), initExprTy)
            | Error e -> Error e
    | If(_, loc, condExpr, thenExpr, elseExpr) ->
        match check env condExpr Type.Bool with
        | Ok tCondExpr ->
            match infer env thenExpr with
            | Ok(tThenExpr, thenTy) ->
                match check env elseExpr thenTy with
                | Ok tElseExpr -> Ok(TypedExpr.If(thenTy, loc, tCondExpr, tThenExpr, tElseExpr), thenTy)
                | Error e -> Error e
            | Error e -> Error e
        | Error e -> Error e        
    | Block(_, loc, exprs) -> typeBlock src env loc exprs
    | Func(_, loc, name, parameters, retType, expr) ->
        let env' =
            (env, parameters)
            ||> List.fold (fun env (paramName, paramTy) ->
                let named = name
                match paramTy with
                | Fun _ -> Env.addFunc env paramName paramTy
                | ty -> Env.addVar env paramName (ty, Immutable))
        
        match check env' expr retType with
        | Ok(TypedExpr.Block(ty, loc, tExprs)) ->
            let tBlock = TypedExpr.Block(ty, loc, tExprs)
            let paramTypes = parameters |> List.unzip |> snd
            let funTy = Type.Fun(paramTypes, retType)
            Ok(TypedExpr.Func(funTy, loc, name, parameters, retType, tBlock), funTy)
        | Error e -> Error e
    | Call(_, loc, name, args) ->
        match Env.lookupFunc env name with
        | Some (Fun(paramTypes, retTy)) ->
            if args.Length = paramTypes.Length then
                (args, paramTypes)
                ||> List.zip
                |> List.map (fun (argTy, paramTy) -> check env argTy paramTy)
                |> List.fold (fun (oks, errors) r ->
                    match r with
                    | Ok value -> oks @ [value], errors
                    | Error e -> oks, errors @ e) ([], [])
                |> function
                    | tExprs, [] -> Ok(TypedExpr.Call(retTy, loc, name, tExprs), retTy)
                    | _, errors -> Error errors
            else
                Error([
                    mkTypeError
                        src
                        loc
                        $"number of arguments does not match the number of parameters (expected {paramTypes.Length}, got {args.Length})."
                        (if args.Length > paramTypes.Length then "too many arguments" else "too few arguments")
                 ])
        | None ->
            Error([
                mkTypeError
                    src
                    loc
                    $"undefined function {name}"
                    $""
            ])
    | While(_, loc, cond, block) ->
        match block with
        | Block(_, loc, exprs) ->
            match check env cond Bool with
            | Ok tCond ->
                match typeBlock src env loc exprs with
                | Ok(tBlock, ty) ->
                    Ok(While(Unit, loc, tCond, tBlock), Unit)
                | Error e -> Error e
            | Error e -> Error e
        | _ ->
            Error([
                mkTypeError
                    src
                    loc
                    $"expected block"
                    $"not a block (???)"
            ])
                
    | Print(_, loc, expr) ->
        match infer env expr with
        | Ok (tExpr, ty) ->
            match ty with
            | I64 | Bool -> Ok(TypedExpr.Print(ty, loc, tExpr), Unit)
            | _ -> Error([
                mkTypeError
                    src
                    loc
                    $"printing unit is not allowed"
                    $"invalid type Unit"
            ])
        | Error e -> Error e
    | _ -> Error(["What the fuck?"])

and checkExpr (src: string list) (env: Env) (expr: UntypedExpr) (expectedTy: Type) =
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
                        $"type mismatch, {typeToStr expectedTy} and {typeToStr exprTy}"
                        $"expected {typeToStr expectedTy}"
                ])
        | Error e -> Error e


let typeCheckCompilationUnit (src: string list) (compUnit: CompilationUnit<unit>): Result<CompilationUnit<Type>, string list> =
    let env = Env.empty
    
    let expr =
        match compUnit with
        | CompilationUnit e -> e
    
    match inferExpr src env expr with
    | Ok (typedExpr, _) -> Ok(CompilationUnit(typedExpr))
    | Error e -> Error e

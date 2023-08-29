module Compiler.Sema.Typechecker

open System.Collections.Generic
open Compiler.AST.ParsedAST
open Compiler.AST.TypedAST
open Compiler.AST.Types
open Compiler.Diagnostics
open Compiler.OptionBuilder

type Mutability =
    | Mutable
    | Immutable

type Env() =
    let variables: Map<Expr, (Type * Mutability)> Stack = Stack()
    member this.addVar name value =
        variables.Pop()
        |> Map.add name value
        |> variables.Push
    member this.lookupVar name =
        let rec inner scopes =
            match scopes with
            | [] -> None
            | scope::scopes' ->
                match Map.tryFind name scope with
                | Some res -> Some res
                | None -> inner scopes'
        inner (variables.ToArray() |> List.ofArray)
    member this.enterScope() = variables.Push Map.empty
    member this.leaveScope() = variables.Pop() |> ignore

let isIntType ty = Seq.contains ty [| I8; I32 |] 

let getTypeWeight = function
    | I8 -> 8
    | I32 -> 32
    | _ -> failwith "uh oh"

let computeType ty1 ty2 =
    if (getTypeWeight ty1) > (getTypeWeight ty2)
    then ty1
    else ty2

let rec typeToStr = function
    | I8 -> "i8"
    | I32 -> "i32"
    | Bool -> "bool"
    | Unit -> "unit"
    | Fn fnTy -> $"""({String.concat " -> " (List.map typeToStr fnTy.Parameters) } -> {typeToStr fnTy.Return})"""
    // | Array(ty, size) -> $"[{typeToStr ty} ; {size}]"

let strToType str =
    match str with
    | "i8" -> Some I8
    | "i32" -> Some I32
    | "unit" -> Some Unit
    | "bool" -> Some Bool
    | _ -> None

let mkTExpr expr loc ty: TExpr = { _expr=expr; Loc=loc; Ty=ty }

// Todo
// * Item Typechecker
//    * Functions
// * CompilationUnit Typechecker
(**
| ExprKind.Func fn ->
        env.addVar fn.Name (fn.TyAnnot, Immutable)
        env.enterScope()
        
        let paramTypes, retTy =
            match fn.TyAnnot with
            | Fun(parameters, retTy) -> parameters, retTy
        
        (fn.Parameters, paramTypes)
        ||> List.zip
        |> List.iter (fun (name, ty) -> env.addVar name (ty, Immutable))
        
        let tBlockOpt = infer fn.Body
        
        env.leaveScope()
        
        match tBlockOpt with
        | Some tBlock ->
            if tBlock.Metadata = retTy then
                Some(mkTExpr (Func {| Name=fn.Name; Parameters=fn.Parameters; Body=tBlock; TyAnnot=fn.TyAnnot |}) expr.Loc Unit)
            else
                // get location of retExpr later on!
                diagnostics.add {
                    Message=($"unexpected return type, expected '{typeToStr retTy}', got '{typeToStr tBlock.Metadata}'")
                    Hint=""
                    Loc=tBlock.Loc
                    Kind=DiagnosticKind.Type
                    Level=DiagnosticLevel.Error
                }
                None
**)
let rec checkType (diagnostics: DiagnosticBag) (env: Env) (expr: Expr) (expectedTy: Type): TExpr option =
    let check = checkType diagnostics env
    let infer = inferType diagnostics env
    let reportTypeError msg hint = diagnostics.add { Message=msg; Hint=hint; Loc=expr.Loc; Kind=DiagnosticKind.Type; Level=DiagnosticLevel.Error }
    
    match expr._expr with
    | _ ->
        match infer expr with
        | Some tyExpr ->
            if tyExpr.Ty = expectedTy
            then Some tyExpr
            else reportTypeError $"type mismatch: expected {typeToStr expectedTy}, got {typeToStr tyExpr.Ty}" ""; None
        | None -> None

and inferType (diagnostics: DiagnosticBag) (env: Env) (expr: Expr): TExpr option =
    let check = checkType diagnostics env
    let infer = inferType diagnostics env
    let reportTypeError msg hint = diagnostics.add { Message=msg; Hint=hint; Loc=expr.Loc; Kind=DiagnosticKind.Type; Level=DiagnosticLevel.Error } // maybe return 'None'?

    match expr._expr with
    | ExprKind.IntegerLiteral num ->
        let numLitWithType = mkTExpr (IntegerLiteral num) expr.Loc
        match num with
        | n when n >= -128 && n <= 127 -> Some(numLitWithType I8)
        // | n when n >= -32768 and n <= 32767 -> Ok(NumLitWithType I16)
        | n when (n >= -2147483648) && n <= 2147483647 -> Some(numLitWithType I32)
        // | n when n >= -9223372036854775808L && n <= 9223372036854775807L -> Some(numLitWithType I64)
        | _ -> reportTypeError "number is outside of the allowable range for i64" ""; None
    | ExprKind.BooleanLiteral b -> Some(mkTExpr (BooleanLiteral b) expr.Loc Bool)
    | ExprKind.Ident name ->
        match env.lookupVar expr with
        | Some(ty, _) -> Some(mkTExpr (Ident name) expr.Loc ty)
        | None -> reportTypeError $"'{name}' is not defined." ""; None
    
    
    | ExprKind.LetVar v ->
        match expr.TypeAnnotation with
        | Some ann -> // Annotated let
            match check v.InitExpr ann with
            | Some typedInitExpr ->
                env.addVar v.Name (ann, Mutability.Mutable)
                Some(mkTExpr (LetVar {| Name=v.Name; InitExpr=typedInitExpr |}) expr.Loc Unit)
            | None -> None
        | None -> // Unannotated let
            match infer v.InitExpr with
            | Some typedInitExpr ->
                env.addVar v.Name (typedInitExpr.Ty, Mutability.Mutable)
                Some(mkTExpr (LetVar {| Name=v.Name; InitExpr=typedInitExpr |}) expr.Loc Unit)

    | ExprKind.ConstVar v ->
        match expr.TypeAnnotation with
        | Some ann -> // Annotated const
            match check v.InitExpr ann with
            | Some typedInitExpr ->
                env.addVar v.Name (ann, Mutability.Immutable)
                Some(mkTExpr (ConstVar {| Name=v.Name; InitExpr=typedInitExpr |}) expr.Loc Unit)
            | None -> None
        | None -> // Unannotated const
            match infer v.InitExpr with
            | Some typedInitExpr ->
                env.addVar v.Name (typedInitExpr.Ty, Mutability.Mutable)
                Some(mkTExpr (ConstVar {| Name=v.Name; InitExpr=typedInitExpr |}) expr.Loc Unit)
    | ExprKind.Assign ass ->
        match env.lookupVar ass.Name with
        | Some (ty, mut) ->
            match mut with
            | Mutable ->
                match check ass.AssExpr ty with
                | Some tyAssExpr -> Some (mkTExpr (Assign {| Name=ass.Name; AssExpr=tyAssExpr |}) expr.Loc ty)
                | None -> None
            | Immutable ->
                reportTypeError $"'trying to assign to immutable variable '{ass.Name}'" "can't assign to an immutable variable"; None
        | None ->
            reportTypeError $"'{ass.Name}' is not defined." ""; None
    | ExprKind.BinOp bin ->
        opt {
            let! typedLeft = infer bin.Left
            let! typedRight = infer bin.Right
            
            let lType = typedLeft.Ty
            let rType = typedRight.Ty
            
            return! (
                match bin.Op with
                | Add | Sub | Mul | Div ->
                    if isIntType lType && isIntType rType then
                        let resTy = computeType lType rType
                        Some(mkTExpr (BinOp({| Op=bin.Op; Left=typedLeft; Right=typedRight |})) expr.Loc resTy)
                    else
                        reportTypeError
                            $"invalid operand types. they have type {typeToStr lType} and {typeToStr rType}, expected ({typeToStr I8}, {typeToStr I32})"
                            $"{typeToStr lType} {binOpStr bin.Op} {typeToStr rType}"
                        ; None
            )
        }
    | ExprKind.IfExpr ifExpr ->
        opt {
            let! tCondExpr = check ifExpr.Cond Bool
            let! tThenExpr = infer ifExpr.Then
            let! tElseExpr = check ifExpr.Else tThenExpr.Ty
            
            return mkTExpr (IfExpr({| Cond=tCondExpr; Then=tThenExpr; Else=tElseExpr |})) expr.Loc tThenExpr.Ty
        }
    | ExprKind.Block block ->
        env.enterScope()
        
        let tBlockOpt =
            block
            |> List.map infer
            |> (fun result ->
                if List.contains None result then
                    None
                else
                    let tExprs = List.choose id result
                    // I should probably change the data structure of blocks (https://stackoverflow.com/a/1175111)
                    match tExprs with
                    | [] -> Some(Block [], Type.Unit)
                    | tExprs ->
                        let retTy = (List.last tExprs).Ty
                        Some(Block tExprs,retTy)
            )
        
        env.leaveScope()
        
        match tBlockOpt with
        | Some (tBlock, retTy) -> Some(mkTExpr tBlock expr.Loc retTy)
        | None -> None

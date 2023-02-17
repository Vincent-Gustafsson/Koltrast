module Koltrast.Interpreter

open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser_utils

type Value =
    | VInt of int
    | VBool of bool
    | VUnit of unit

type Env = {
    vars: Map<string, Value>
    funcs: Map<string, TypedExpr>
}

module Env =
    let empty = { vars=Map.empty; funcs=Map.empty }
    let addVar env key value = { env with vars=(Map.add key value env.vars) }
    let lookupVar env key = Map.find key env.vars
    let addFunc env key value = { env with funcs=(Map.add key value env.funcs) }
    let lookupFunc env key = Map.find key env.funcs

let evaluateProgram (compUnit: CompilationUnit<Type>) =
    let rec evaluate (env: Env) expr =
        match expr with
        | NumericLiteral(_, _, n) -> env, VInt n
        | BoolLiteral(_, _, b) -> env, VBool b
        | Ident(_, _, name) -> env, Env.lookupVar env name
        | BinOp(_, _, op, l, r) ->
            let (env', lVal) = evaluate env l
            let (env'', rVal) = evaluate env' r
            
            let vRes =
                match lVal, rVal with
                | VInt n1, VInt n2 ->
                    match op with
                    | Add -> VInt(n1 + n2)
                    | Sub -> VInt(n1 - n2)
                    | Mul -> VInt(n1 * n2)
                    | Div -> VInt(n1 / n2)
            
            env'', vRes
        | Assign(_, _, ident, assExpr) ->
            let name = getNameFromIdent ident
            let env', assVal = evaluate env assExpr
            Env.addVar env' name assVal, assVal 
        | AnnVarDecl(ty, _, name, _, _, exprOption) ->
            match exprOption with
            | Some initExpr ->
                let env', initVal = evaluate env initExpr
                Env.addVar env' name initVal, VUnit()
            | None ->
                match ty with
                | Int -> Env.addVar env name (VInt(0)), VUnit()
                | Bool -> Env.addVar env name  (VBool(false)), VUnit()
        | InferredVarDecl(_, _, name, _, _, initExpr) ->
            let env', initVal = evaluate env initExpr
            Env.addVar env' name initVal, VUnit()
        | If(_, _, condExpr, thenExpr, elseExpr) ->
            let env', condVal = evaluate env condExpr
            match condVal with
            | VBool condBool ->
                if condBool = true then
                    evaluate env' thenExpr
                else
                    evaluate env' elseExpr
        | Block(_, _, exprs) ->
            ((env, []), exprs)
            ||> List.fold (fun (env, vals) expr ->
                let env', exprVal = evaluate env expr
                env', vals @ [exprVal])
            |> (fun (env', vals) ->
                env', List.last vals)
        | Func(_, _, name, parameters, _, block) ->
            Env.addFunc env name block, VUnit()
            
    match compUnit with
    | CompilationUnit expr -> evaluate Env.empty expr
        
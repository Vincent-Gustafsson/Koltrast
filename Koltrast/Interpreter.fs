module Koltrast.Interpreter

open Koltrast.Frontend.AST
open Koltrast.Frontend.Parser_utils

type Value =
    | VInt of int
    | VBool of bool
    | VUnit of unit

type Env = {
    vars: Map<string, Value>
    funcs: Map<string, (string list * TypedExpr)>
}

module Env =
    let empty = { vars=Map.empty; funcs=Map.empty }
    let extendWith (env1: Env) (env2: Env) =
        let vars' =
            (env1.vars, env2.vars)
            ||> (fun a b -> Map.toList a, Map.toList b)
            ||> (@)
            |> Map.ofList
        
        let funcs' =
            (env1.funcs, env2.funcs)
            ||> (fun a b -> Map.toList a, Map.toList b)
            ||> (@)
            |> Map.ofList
            
        { vars=vars'; funcs=funcs' }
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
                    | Mod -> VInt(n1 % n2)
                    | GT -> VBool(n1 > n2)
                    | LT -> VBool(n1 < n2)
                    | EQ -> VBool(n1 = n2)
                | VBool b1, VBool b2 ->
                    match op with
                    | EQ -> VBool(b1 = b2)
            
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
                match vals with
                | [] -> env', VUnit()
                | vals -> env', List.last vals)
        | Func(_, _, name, params, _, block) ->
            Env.addFunc env name (params |> List.unzip |> fst, block), VUnit()
        | Call(_, _, name, args) ->
            let (params, block) = Env.lookupFunc env name
            let (env', argValues) =
                ((env, []), args)
                ||> List.fold (fun (env, values) expr ->
                    let env', value = evaluate env expr
                    env', values @ [value])
            
            let paramVars =
                (params, argValues)
                ||> List.zip
                |> Map.ofList
                
            let env'' = Env.extendWith env' { vars=paramVars; funcs=Map.empty }
            
            let _, callResultValue = evaluate env'' block
            
            env, callResultValue
        | While(_, _, cond, block) ->
            let rec inner env =
                match evaluate env cond |> snd with
                | VBool b ->
                    if b then
                        let env', _ = evaluate env block
                        inner env'
                    else
                        ()
            
            inner env
            env, VUnit()
        | Print(_, _, expr) ->
            let env', value = evaluate env expr
            match value with
            | VInt n -> printfn $"{n}"
            | VBool b -> printfn $"{b}"
            env', value
            
            
    match compUnit with
    | CompilationUnit expr -> evaluate Env.empty expr
        
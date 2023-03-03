module Koltrast.Frontend.GenerateIR

open Koltrast.Frontend.AST
open Koltrast.Frontend.IR
open Koltrast.Frontend.IRBuilder

let getEntrypointName expr =
    match expr._expr with
    | Func fn -> fn.Name

let expectOpnd (opndOpt: Option<Operand>): Operand =
    match opndOpt with
    | Some opnd -> opnd
    | None -> failwith "expected operand"

let rec generate (builder: IRBuilder) expr =
    let gen = generate builder
    let exprTy = expr.Metadata
    match expr._expr with
    | NumericLiteral num -> Some { _opnd=IntConst num; Ty=exprTy }
    | BoolLiteral b -> Some { _opnd=BoolConst b; Ty=exprTy }
    | Ident name ->
        let opnd = { _opnd=Symbol name; Ty=exprTy }
        builder.addVar name opnd
        Some opnd
    | BinOp bin ->
        let fst = expectOpnd (gen bin.Left)
        let snd = expectOpnd (gen bin.Right)
        let dst = { _opnd=builder.GenTempVar(); Ty=exprTy }
        builder.Emit { _instr=BinInstr {| Op=bin.Op; Dst=dst; Fst=fst; Snd=snd |}; Ty=exprTy }
        Some dst
    | Func fn ->
        let retTy, parameters =
            match fn.TyAnnot with
            | Fun(paramTypes, retTy) ->
                retTy, List.zip fn.Parameters paramTypes
        
        builder.BeginFunction fn.Name retTy parameters
        
        match fn.Body._expr with
        | Block exprs ->
            match exprs with
            | [] ->
                builder.Emit { _instr=Return None; Ty=Unit }
            | [lastExpr] ->
                let retSym = gen lastExpr
                builder.Emit { _instr=Return retSym; Ty=fn.Body.Metadata }
            | exprs ->
                exprs
                |> List.map gen
                |> List.last
                |> (fun retSym -> builder.Emit { _instr=Return retSym; Ty=fn.Body.Metadata })
        
        builder.EndFunction()
        None
    | Entrypoint fn ->
        builder.SetEntrypoint (getEntrypointName fn)
        gen fn // Since this is a function it'll return a None
    | _ -> failwith $"incomplete match case IR: ({expr._expr})"
    
let generateIR exprs =
    let builder = IRBuilder()
    List.map (generate builder) exprs
    builder.GetIR()

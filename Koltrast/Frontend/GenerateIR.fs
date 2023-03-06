module Koltrast.Frontend.GenerateIR

open Koltrast.Frontend.AST
open Koltrast.Frontend.IR
open Koltrast.Frontend.IRBuilder



let expectOpnd opndOpt: Operand =
    match opndOpt with
    | Some opnd -> opnd
    | None -> failwith "expected operand"

let UNIT_OPND = { _opnd=UnitConst; Ty=Unit }

let rec generate (builder: IRBuilder) (expr: TypedExpr): Option<Operand> =
    let gen = generate builder
    let exprTy = expr.Metadata
    
    match expr._expr with
    | NumericLiteral num -> Some { _opnd=IntConst num; Ty=exprTy }
    | BoolLiteral b -> Some { _opnd=BoolConst b; Ty=exprTy }
    | BinOp bin ->
        let fstOpnd = gen bin.Left |> expectOpnd
        let sndOpnd = gen bin.Right |> expectOpnd
        let dstOpnd = builder.GenTempVar(exprTy)
        builder.Emit { _instr=Bin {| Op=bin.Op; Fst=fstOpnd; Snd=sndOpnd; Dst=dstOpnd |}; Ty=exprTy }
        Some dstOpnd
    | Ident name ->
        let srcOpnd = builder.GetVar(name)
        let dstOpnd = builder.GenTempVar(exprTy)
        builder.Emit { _instr=Load {| Src=srcOpnd; Dst=dstOpnd |}; Ty=exprTy }
        Some dstOpnd
    | Assign ass ->
        builder.GetVar(ass.Name) |> ignore
        let srcOpnd = gen ass.AssignExpr |> expectOpnd
        let dstOpnd = builder.GenTempVar(exprTy)
        builder.Emit { _instr=Store {| Dst=dstOpnd; Src=srcOpnd |}; Ty=exprTy }
        Some dstOpnd
    | Var v ->
        match v.InitExprOpt with
        | Some initExpr ->
            let initTy = initExpr.Metadata
            let dstOpnd = builder.GenTempVar(initTy)
            let initOpnd = gen initExpr |> expectOpnd
            builder.Emit { _instr=Alloc {| Dst=dstOpnd; Ty=initTy|}; Ty=initTy }
            builder.Emit { _instr=Store {| Dst=dstOpnd; Src=initOpnd |}; Ty=initTy }
            builder.AddVar(v.Name, initTy)
        | None ->
            let ty = v.TyAnnot.Value
            let dstOpnd = builder.GenTempVar(ty)
            builder.Emit { _instr=Alloc {| Dst=dstOpnd; Ty=ty|}; Ty=ty }
            builder.AddVar(v.Name, ty)
        |> ignore
        
        Some UNIT_OPND
    | Block bl ->
        builder.EnterScope()
        
        let lastOpnd =
            match bl with
            | [] -> UNIT_OPND
            | exprs ->
                exprs
                |> List.map gen
                |> List.last
                |> function
                    | Some opnd -> opnd
                    | None -> UNIT_OPND
        
        builder.LeaveScope()
        Some lastOpnd
    | Func fn ->
        let paramTypes, retTy =
            match fn.TyAnnot with
            | Fun(paramTypes, retTy) -> paramTypes, retTy
        
        let parameters =
            (fn.Parameters, paramTypes)
            ||> List.zip
            |>  List.map Parameter
        
        builder.AddVar(fn.Name, exprTy) |> ignore
        
        builder.EnterFunction({
            Name=fn.Name
            Parameters=parameters
            ReturnType=retTy
            Blocks=[]
        })
        
        match fn.Body._expr with
        | Block exprs ->
            match exprs with
            | [] ->
                builder.Emit { _instr=Return UNIT_OPND; Ty=Unit }
            | exprs ->
                exprs
                |> List.map gen
                |> List.last
                |> (fun retOpnd -> builder.Emit { _instr=Return(expectOpnd retOpnd); Ty=fn.Body.Metadata })

        builder.LeaveFunction()
        Some UNIT_OPND
    | Entrypoint expr ->
        let fn = match expr._expr with | Func fn -> fn
        builder.SetEntrypoint(fn.Name)
        gen expr |> ignore
        None
    
    | _ -> failwith $"unimplemented IR: ({expr._expr})"
TEMPVAR problem. Scope / Block / ???
let generateIR exprs =
    let builder = IRBuilder()
    // global scope, I think?
    builder.EnterScope()
    List.map (generate builder) exprs |> ignore
    builder.LeaveScope()
    builder.GetIR()

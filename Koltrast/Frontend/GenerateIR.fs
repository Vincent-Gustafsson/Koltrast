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
        
        builder.EnterFunction({
            Name=fn.Name
            Parameters=parameters
            ReturnType=retTy
            Blocks=[]
        })
        
        

let generateIR exprs =
    let builder = IRBuilder()
    List.map (generate builder) exprs
    builder.GetIR()

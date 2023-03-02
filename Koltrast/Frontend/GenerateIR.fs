module Koltrast.Frontend.GenerateIR

open System.Collections.Generic
open Koltrast.Frontend.AST
open Koltrast.Frontend.IR
            
type IRBuilder() =
    let mutable entrypoint: Option<Symbol> = None
    let mutable functions: Function list = []
    let mutable currFnIdx = -1
    let mutable nextIsEp = false
    
    member this.setEntryPoint() = nextIsEp <- true
    
    member this.beginFunction fnName parameters initalBlocks =
        if nextIsEp then
            entrypoint <- Some fnName
        else ()
        let fn = { Name=fnName; Parameters=parameters; Blocks=initalBlocks }
        currFnIdx <- currFnIdx + 1
        functions <- functions @ [fn]
        fn
    member this.exitFunction() = currFnIdx <- currFnIdx - 1
    
    member this.emit instr =
        let fn = functions[currFnIdx]
        fn.Blocks[currBlo]

let expectOperand = function
    | Some op -> op
    | None -> failwith "__INTERNAL_IR_ERROR__: expected operand"

let rec gen builder expr (blockName: Option<string>): Option<Operand> =
    let genReturnOperand expr = gen builder expr blockName |> expectOperand
    let genInBlockReturnOperand expr blockName' = gen builder expr blockName' |> expectOperand
    
    let exprTy = expr.Metadata
    match expr._expr with
    | NumericLiteral num -> Some({ Value=IntConst num; Ty=exprTy })
    | BoolLiteral b -> Some({ Value=BoolConst b; Ty=Bool })
    | ExprKind.BinOp bin ->
        let dst = builder.genTempVar()
        let fstOp = genReturnOperand bin.Left
        let sndOp = genReturnOperand bin.Right
        
        builder.emit { Value=(BinOp {| Op=bin.Op; Dst=dst; Fst=fstOp; Snd=sndOp |}); Type=exprTy }
        
        Some dst
    | Ident name ->
        let sym = builder.getVar name
        Some sym
    | Assign ass ->
        let dst = builder.genNewVar name
        let fst = genReturnOperand ass.AssignExpr
        builder.emit { Value=(Store {| Dst=dst; Fst=fst |}); Type=exprTy }
    | Block bl ->
        let retSym =
            match bl with
            | [] -> None
            | [retExpr] ->
                let sym = genReturnOperand retExpr
                Some sym
            | blockExprs ->
                blockExprs
                |> List.map (fun e -> gen builder e blockName)
                |> List.last
        retSym
    | ExprKind.Func fn ->
        let entryBlock = { Name="entry"; Instructions=[] }
        let irFn = builder.beginFunction fn.Name fn.Parameters [entryBlock]
        builder.enterFunction()
        let retSym = genInBlockReturnOperand fn.Body (Some("entry"))
        builder.emit { Value=(Return retSym); Type=exprTy }
        builder.exitFunction()
        None
    | Entrypoint fn ->
        builder.setEntrypoint()
        gen builder fn None |> ignore
        None
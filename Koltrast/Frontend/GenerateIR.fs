module Koltrast.Frontend.GenerateIR

open System.Collections.Generic
open Koltrast.Frontend.AST
open Koltrast.Frontend.IR
            
type IRBuilder() =
    let mutable vars: Map<string, (int * Operand)> Stack = Stack()
    let mutable entrypoint: Option<Symbol> = None
    let mutable functions: Function list = []
    let mutable currFnIdx = -1
    let mutable nextIsEp = false
    let mutable currBlock = -1
    let mutable tempVar = -1
    member this.setEntrypoint() = nextIsEp <- true
    
    member this.beginFunction fnName parameters initalBlocks =
        vars.Push Map.empty
        if nextIsEp then
            entrypoint <- Some fnName
        else ()
        let fn = { Name=fnName; Parameters=parameters; Blocks=initalBlocks }
        currFnIdx <- currFnIdx + 1
        currBlock <- currBlock + 1
        functions <- functions @ [fn]
        fn
    member this.exitFunction() = currFnIdx <- currFnIdx - 1; currBlock <- currBlock - 1
    
    member this.emit instr =
        let fn = functions[currFnIdx]
        let fn' = { fn with Blocks=(List.mapi (fun i bl ->
            if i = currBlock then
                { bl with Instructions=(bl.Instructions @ [instr]) }
            else bl) fn.Blocks)
        }
        functions <- (List.mapi (fun i f -> if currFnIdx = i then fn' else f) functions)
    
    member this.genTempVar(): OperandKind = tempVar <- tempVar + 1; Symbol $"t{tempVar}"

    member this.getVar name =
        let rec find scopes =
            match scopes with
            | [] -> failwith "umm???"
            | scope :: rest -> 
                match Map.tryFind name scope with
                | Some res -> res |> snd
                | None -> find rest
        
        vars.ToArray()
        |> List.ofArray
        |> List.rev
        |> find
    member this.genNewVar name ty: Operand =
        match Map.tryFind name (vars.Peek()) with
        | Some(version, _) ->
            let version' = version + 1
            let operand': Operand = { Value=(Symbol ("%" + $"{name}_{version'}")); Ty=ty }
            let scope = vars.Pop()
            let scope' = Map.add name (version', operand') scope
            vars.Push scope'
            vars <- vars
            operand'
        | None ->
            let operand: Operand = { Value=(Symbol("%" + $"{name}_{0}")); Ty=ty }
            let scope = vars.Pop()
            let scope' = Map.add name (0, operand) scope
            vars.Push scope'
            vars <- vars
            operand
            
    member this.getIR() = (entrypoint, functions)

let expectOperand opOpt: Operand =
    match opOpt with
    | Some op -> op
    | None -> failwith "__INTERNAL_IR_ERROR__: expected operand"

let rec gen (builder: IRBuilder) expr (blockName: Option<string>): Option<Operand> =
    let genReturnOperand expr = gen builder expr blockName |> expectOperand
    let genInBlockReturnOperand expr blockName' = gen builder expr blockName' |> expectOperand
    
    let exprTy = expr.Metadata
    match expr._expr with
    | NumericLiteral num -> Some({ Value=IntConst num; Ty=exprTy })
    | BoolLiteral b -> Some({ Value=BoolConst b; Ty=Bool })
    | ExprKind.BinOp bin ->
        let dstSym = builder.genTempVar()
        let dst: Operand = { Value=dstSym; Ty=exprTy }
        let fstOp = genReturnOperand bin.Left
        let sndOp = genReturnOperand bin.Right
        
        builder.emit { Value=(BinOp {| Op=bin.Op; Dst=dst; Fst=fstOp; Snd=sndOp |}); Ty=exprTy }
        
        Some dst
    | Ident name ->
        let sym = builder.getVar name
        Some sym
    | Assign ass ->
        let dst = builder.genNewVar ass.Name exprTy
        let fst = genReturnOperand ass.AssignExpr
        builder.emit { Value=(Store {| Dst=dst; Fst=fst |}); Ty=exprTy }
        Some dst
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
        let parameters =
            match fn.TyAnnot with
            | Fun(paramTypes, _) -> List.zip fn.Parameters paramTypes
            | _ -> failwith $"shouldn't happen: {exprTy}"
        
        let entryBlock = { Name="entry"; Instructions=[] }
        let irFn = builder.beginFunction fn.Name parameters [entryBlock]
        builder.genNewVar fn.Name fn.TyAnnot |> ignore
        List.iter (fun (name, ty) -> builder.genNewVar name ty |> ignore) parameters
        let retSym = gen builder fn.Body (Some("entry"))
        builder.emit { Value=(Return retSym); Ty=exprTy }
        builder.exitFunction()
        None
    | Entrypoint fn ->
        builder.setEntrypoint()
        gen builder fn None |> ignore
        None
    | _ -> failwith $"unimplemented ir generation for: {expr._expr}"

let generateIr exprs =
    let builder = IRBuilder()
    List.map (fun e -> gen builder e None) exprs
    builder.getIR()

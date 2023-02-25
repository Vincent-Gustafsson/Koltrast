module Koltrast.Middleend.IR

open Koltrast.Frontend.AST

type Instruction =
    | BinOp of {| Op: BinOpKind; Fst: Operand; Snd: Operand; Dst: Operand |}
    | Literal of {| Value: Operand; Dst: Operand |}
    | Set of {| Dst: Operand; Value:Operand |}
    | Load of {| Symbol: Operand; Dst: Operand; |}
    override x.ToString() =
        match x with
        | BinOp bin -> $"{bin.Dst} = {bin.Fst} {binOpStr bin.Op} {bin.Snd}"
        | Literal lit -> $"{lit.Dst} = {lit.Value}"
        | Set s -> $"{s.Dst} = {s.Value}"

and Operand =
    | ConstUnit
    | ConstI8 of int64
    | ConstI64 of int64
    | ConstBool of bool
    | Symbol of string
    override x.ToString() =
        match x with
        | ConstUnit -> "()"
        | ConstI8 num -> $"{num}"
        | ConstI64 num -> $"{num}"
        | ConstBool b -> $"{b}"
        | Symbol name -> name
        
and Label = { Name: string; Instructions: Instruction list }

type IRBuilder() =
    let mutable symbolTable: (string list) list = [[]]
    let mutable currentScope = -1
    let mutable instructions: Instruction list = []
    
    member this.emit (instr: Instruction) =
        instructions <- instructions @ [instr]

    member this.genInstrName() = Operand.Symbol("%" + $"{instructions.Length + 1}")
    
    member this.enterScope() =
        currentScope <- currentScope + 1
        if currentScope > symbolTable.Length then
            symbolTable <- symbolTable @ [[]]
    member this.exitScope() = currentScope <- currentScope - 1
    member this.addVar string =
        let scope = symbolTable[currentScope]
        let scope' = scope @ [string]
        symbolTable <- (List.mapi (fun i scope -> if currentScope = i then scope' else scope) symbolTable)
        
    member this.getIR() = instructions

let generateIR expr =
    let ensureSymbol symOpt: Operand =
        match symOpt with
        | Some sym -> sym
        | None -> failwith "What the fuck???"

    let builder = new IRBuilder()
    let rec gen expr: Option<Operand> =
        match expr._expr with
        | ExprKind.NumericLiteral num ->
            let sym = builder.genInstrName()
            builder.emit (Instruction.Literal {| Value=ConstI64 num; Dst=sym |})
            Some sym
        | ExprKind.BoolLiteral b ->
            let sym = builder.genInstrName()
            builder.emit (Instruction.Literal {| Value=ConstBool b; Dst=sym |})
            Some sym
        | ExprKind.BinOp bin ->
            let left = ensureSymbol (gen bin.Left)
            let right = ensureSymbol (gen bin.Right)
            let resultSym = builder.genInstrName()
            builder.emit (Instruction.BinOp {| Op=bin.Op; Fst=left; Snd=right; Dst=resultSym |})
            Some resultSym
        | Block exprs ->
            builder.enterScope()
            let sym = 
                match exprs with
                | [] -> Some(Operand.ConstUnit)
                | [expr] -> Some(ensureSymbol (gen expr))
                | allExprs ->
                    let allSymbols = allExprs |> List.map (fun sym -> (ensureSymbol(gen sym)))
                    Some(List.last allSymbols)
            builder.exitScope()
            sym
        | Var v ->
            builder.addVar v.Name
            match v.InitExprOpt with
            | Some initExpr ->
                let initExprSym = ensureSymbol (gen initExpr)
                builder.emit (Set({| Dst=Symbol v.Name; Value=initExprSym |}))
                Some(Operand.ConstUnit)
            | None ->
                Some(Operand.ConstUnit)
        | Ident id -> Some(Symbol id)
                
    gen expr |> ignore
    builder.getIR()

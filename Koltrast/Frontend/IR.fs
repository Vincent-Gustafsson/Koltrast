module Koltrast.Frontend.IR

open Koltrast.Frontend.AST

type OperandKind =
    | UnitConst
    | IntConst of int64
    | BoolConst of bool
    | TempVar of string
    // | Label of string
    override x.ToString() =
        match x with
        | UnitConst -> "()"
        | IntConst num -> num.ToString()
        | BoolConst b -> b.ToString()
        |TempVar name -> name

type Operand =
    { _opnd: OperandKind; Ty: Type }
    override x.ToString() = $"{x._opnd}"

type InstructionKind =
    | Bin of {| Op: BinOpKind; Fst: Operand; Snd: Operand; Dst: Operand |}
    | Load of {| Src: Operand; Dst: Operand |}
    | Store of {| Dst: Operand; Src: Operand |}
    | Alloc of {| Ty: Type; Dst: Operand |}
    | Call of {| Name: string; Arguments: Operand list; Dst: Operand |}
    | Return of Operand
    override x.ToString() =
        match x with
        | Bin bin -> $"{bin.Dst} = {bin.Fst} {binOpStr bin.Op} {bin.Dst}"
        | Load l -> $"load {l.Dst}, {l.Src}"
        | Store s -> $"store {s.Dst}, {s.Src}"
        | Alloc a -> $"{a.Dst} = alloca {typeToStr a.Ty}"
        | Call c -> failwith "not implemented yet."
        | Return retOpnd -> $"ret {retOpnd}"

type Instruction =
    { _instr: InstructionKind; Ty: Type  }
    // Shows instr Type: override x.ToString() = $"{x._instr} ({typeToStr x.Ty})"
    override x.ToString() = $"{x._instr}"
        
type BasicBlock = {
    Name: string
    mutable Instructions: Instruction list
    mutable Predecessors: BasicBlock list
    mutable Successors: BasicBlock list
    // PhiNodes: PhiNode list
}

type Parameter = string * Type

type Function = {
    Name: string
    Parameters: Parameter list
    ReturnType: Type
    mutable Blocks: BasicBlock list
}

type Program = {
    Entrypoint: string
    Functions: Function list
}

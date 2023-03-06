module Koltrast.Frontend.IR

open Koltrast.Frontend.AST

type OperandKind =
    | UnitConst
    | IntConst of int64
    | BoolConst of bool
    | TempVar of string
    // | Label of string

type Operand = { _opnd: OperandKind; Ty: Type }

type InstructionKind =
    | Bin of {| Op: BinOpKind; Fst: Operand; Snd: Operand; Dst: Operand |}
    | Load of {| Src: Operand; Dst: Operand |}
    | Store of {| Dst: Operand; Src: Operand |}
    | Alloc of {| Ty: Type; Dst: Operand |}
    | Call of {| Name: string; Arguments: Operand list; Dst: Operand |}
    | Return of Operand

type Instruction = { _instr: InstructionKind; Ty: Type  }

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

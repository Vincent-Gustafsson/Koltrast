module Tests.ParseTests.ParsedASTConstruction

open Compiler.AST.ParsedAST

let private _loc = {
    StreamName = "testing"
    Start = {| Index=0; Line=0; Col=0 |} 
    End = {| Index=0; Line=0; Col=0 |} 
}

let private mkExpr kind = {
    _expr = kind
    Loc = _loc
    TypeAnnotation = None 
}

let id name = mkExpr (Ident name)
let num n = mkExpr (IntegerLiteral n)
let bool b = mkExpr (BooleanLiteral b)
let err errMsg = mkExpr (Error errMsg)

let bin op l r = mkExpr (BinOp {| Op=op; Left=l; Right=r |})
let ifelse cond t e = mkExpr (IfExpr {| Cond=cond; Then=t; Else=e |})
let while_ cond body = mkExpr (While {| Cond=cond; Body=body |})

let let_ name initExpr = mkExpr (LetVar {| Name=id name; InitExpr=initExpr |})
let letAnn_ name ty initExpr = {
    _expr = LetVar {| Name=id name; InitExpr=initExpr |}
    Loc = _loc
    TypeAnnotation = Some ty 
}
let const_ name initExpr = mkExpr (ConstVar {| Name=id name; InitExpr=initExpr |})
let constAnn_ name ty initExpr = {
    _expr = ConstVar {| Name=id name; InitExpr=initExpr |}
    Loc = _loc
    TypeAnnotation = Some ty 
}

let ass name assExpr = mkExpr (Assign {| Name=id name; AssExpr=assExpr |})
let appl name args = mkExpr (FuncAppl {| Name=id name; Arguments=args |})

let block exprs = mkExpr (Block exprs)

let fn name parameters retType body =
    let parameters: FnParam list =
        parameters
        |> List.map (fun (name, ty) -> {| Name=id name; Ty=ty |})
    
    let fnItem = Function {|
        Name = id name
        Params = parameters
        ReturnType = retType
        Body = body
    |}
    { _item=fnItem; Loc=_loc }

let prog functions = { Items=functions }

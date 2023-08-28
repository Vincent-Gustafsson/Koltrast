module Compiler.Parse.Parser

open Compiler.AST.ParsedAST
open Compiler.AST.Types
open FParsec

let isValidIdentChar c =
    ['_'] @ ['A'..'Z'] @ ['a'..'z']
    |> Seq.exists (fun ch -> ch = c)

let isKeyword str =
    [| "true"; "false"; "let"; "const"; "fn"; "while"; "if"; "then"; "else" |]
    |> Seq.exists (fun kw -> str = kw)

let ws = spaces

let mkExprWithAnnotation loc exprKind tyAnnotOpt = { _expr=exprKind; Loc=loc; TypeAnnotation=tyAnnotOpt }
let mkExpr loc exprKind = { _expr=exprKind; Loc=loc; TypeAnnotation=None }

type SymbolKind =
    | Dot
    | Comma
    | Colon
    | Semicolon
    | OpenCurly
    | CloseCurly
    | OpenParen
    | CloseParen
    | OpenBracket
    | CloseBracket
    | RArrow

// Parse a symbol and then strip trailing ws
let symbol sym =
    (match sym with
    | Dot -> skipString "."
    | Comma -> skipString ","
    | Colon -> skipString ":"
    | Semicolon -> skipString ";"
    | OpenCurly -> skipString "{"
    | CloseCurly -> skipString "}"
    | OpenParen -> skipString "("
    | CloseParen -> skipString ")"
    | OpenBracket -> skipString "["
    | CloseBracket -> skipString "]"
    | RArrow -> skipString "->"
    .>> ws)

type KeywordKind =
    | True
    | False
    | Let
    | Const
    | Fn
    | While
    | If
    | Then
    | Else

// Parse a symbol and then strip trailing ws
let keyword kw =
    (match kw with
    | True -> skipString "true"
    | False -> skipString "false"
    | Let -> skipString "let"
    | Const -> skipString "const"
    | Fn -> skipString "fn"
    | While -> skipString "while"
    | If -> skipString "if"
    | Then -> skipString "then"
    | Else -> skipString "else"
    .>> ws)

let pTyAnnotWithoutColon = choice [
    (stringReturn "i8" (Type.I8))
    (stringReturn "i32" (Type.I32))
    (stringReturn "bool" (Type.Bool))
    (stringReturn "unit" (Type.Unit))
]

let pTyAnnot = symbol Colon >>. pTyAnnotWithoutColon

let betweenParens p = between (symbol OpenParen) (symbol CloseParen) p

let locFromFParsecPos (startPos: Position) (endPos: Position) = {
    StreamName = startPos.StreamName
    Start = {| Index=int startPos.Index; Line=int startPos.Line; Col=int startPos.Column |}
    End = {| Index=int endPos.Index; Line=int endPos.Line; Col=int endPos.Column |}
}

let combineLocations loc1 loc2 = {
    StreamName = loc1.StreamName
    Start = loc1.Start
    End = loc2.End
}

let getLoc p = parse {
    do! ws
    let! startPos = getPosition
    let! pRes = p
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    return (loc, pRes)
}

let pIdent =
    getLoc (many1Satisfy isValidIdentChar .>> ws) <?> "identifier"
    >>= (fun (loc, name) ->
        if isKeyword name then
            fail $"'{name}' is a reserved keyword"
        else
            preturn (mkExpr loc (Ident(name)))
    )

let pInt = getLoc pint64 .>> ws |>> (fun (loc, n) -> mkExpr loc (IntegerLiteral(int n)))
let pBool = getLoc (stringReturn "true" true <|> stringReturn "false" false) .>> ws |>> (fun (loc, b) ->
    mkExpr loc (BooleanLiteral(b)))

// Operators
let opp = OperatorPrecedenceParser<Expr,_,unit>()
let expr = opp.ExpressionParser

let addInfixOperator str prec assoc (mapping: Location -> Expr -> Expr -> Expr) =
    let op = InfixOperator(str, ws >>. getPosition, prec, assoc, (),
        (fun _ l r -> mapping (combineLocations l.Loc r.Loc) l r))
    (opp.AddOperator(op))

let mkBinOpExpr op loc l r = mkExpr loc (ExprKind.BinOp({|  Op=op; Left=l; Right=r  |}))

addInfixOperator "+" 7 Associativity.Left (mkBinOpExpr Add)
addInfixOperator "-" 7 Associativity.Left (mkBinOpExpr Sub)
addInfixOperator "*" 8 Associativity.Left (mkBinOpExpr Mul)
addInfixOperator "/" 8 Associativity.Left (mkBinOpExpr Div)

addInfixOperator "=" 3 Associativity.Right (fun loc id assExpr ->
    match id._expr with
    | Ident _ -> mkExpr loc (Assign({| Name=id; AssExpr=assExpr |}))
    | _ -> mkExpr loc (ExprKind.Error "l-value of assign must be an identifier"))

let pBlock =
    getLoc (
        between
            (symbol OpenCurly)
            (symbol CloseCurly)
            (many expr))
    |>> (fun (loc, exprs) -> mkExpr loc (Block(exprs)))

let pLetVar = parse {
    let! startPos = getPosition
    do! (keyword Let) <?> "'let' keyword in variable declaration."
    
    let! id = pIdent
    let! tyAnnOpt = opt pTyAnnot
    
    do! skipChar '=' <?> "'=' in variable declaration"
    
    let! initExpr = expr
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos

    return mkExprWithAnnotation loc (LetVar {| Name=id; InitExpr=initExpr |}) tyAnnOpt
}

let pConstVar = parse {
    let! startPos = getPosition
    do! (keyword Const) <?> "'const' keyword in variable declaration."
    
    let! id = pIdent
    let! tyAnnOpt = opt pTyAnnot
    
    do! skipChar '=' <?> "'=' in variable declaration"
    
    let! initExpr = expr
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos

    return mkExprWithAnnotation loc (ConstVar {| Name=id; InitExpr=initExpr |}) tyAnnOpt
}

let pVar = pLetVar <|> pConstVar

// parse an identifier or a function call.
// Probably not the best way of doing this.
// But it works!
let pIdentWithOptArgs =
    getLoc (tuple2 pIdent (opt (betweenParens (sepBy (ws >>. expr) (ws >>. skipChar ',')))))
    |>> (fun (loc, (id, optArgs)) ->
            match optArgs with
            | Some args -> mkExpr loc (FuncAppl({| Name=id; Arguments=args |}))
            | None -> id)

let pFunc = parse {
    do! ws
    
    let! funcStartPos = getPosition
    do! keyword Fn

    let! name = pIdent
    let! parameters = betweenParens (sepBy (pIdent .>>. pTyAnnot) (symbol Comma))
    
    do! symbol RArrow <?> "return type"
    let! retType = pTyAnnotWithoutColon
    let! body = pBlock

    let! funcEndPos = getPosition
    let loc = locFromFParsecPos funcStartPos funcEndPos
    
    return (
        let parameters =
            parameters
            |> List.map (fun (name, ty) -> {| Name=name; Ty=ty |})

        {
            Loc=loc
            _item = (
                Function {|
                    Name=name
                    Params=parameters
                    ReturnType=retType
                    Body=body
                |})
        })
}

let pWhile =
    getLoc (
        keyword While
        >>. expr <?> "while condition"
        .>>. pBlock  <?> "while block"
    ) <?> "while loop"
    |>> (fun (loc, (cond, block)) -> mkExpr loc (ExprKind.While({| Cond=cond; Body=block |})))

let pIf = parse {
    do! ws
    let! startPos = getPosition
    
    do! keyword If
    let! cond = expr

    do! keyword Then
    let! thenExpr = expr

    do! keyword Else
    let! elseExpr = expr

    let! endPos = getPosition
    let loc = locFromFParsecPos startPos endPos
    
    return mkExpr loc (IfExpr {| Cond=cond; Then=thenExpr; Else=elseExpr |})
}

opp.TermParser <- choice [
    attempt pIdentWithOptArgs // identifier or function application
    attempt pInt
    attempt pBool
    betweenParens expr   
    
    attempt pIf
    pBlock
    pWhile
    pVar
]

let pProgram = ws >>. many pFunc .>> eof |>> (fun items -> { Items=items })

let runProgramParserOnStr streamName input =
    match runParserOnString pProgram () streamName input with
    | Success(ast, _, _) -> Result.Ok ast
    | Failure(errStr, _, _) -> Result.Error errStr

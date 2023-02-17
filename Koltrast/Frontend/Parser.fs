module Koltrast.Frontend.Parser

open FParsec
open AST
open Parser_utils

let ws = spaces

let getLoc p = parse {
    do! ws
    let! startPos = getPosition
    let! pRes = p
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    return (loc, pRes)
}

let thingy loc1 loc2 = {
    StreamName = loc1.StreamName
    Start = {| Index=int loc1.Start.Index; Line=int loc1.Start.Line; Col=int loc1.Start.Col |}
    End = {| Index=int loc2.End.Index; Line=int loc2.End.Line; Col=int loc2.End.Col |}
}

// Keywords
let pmut = stringReturn "mut" Mutable <|>% Immutable
let plet = skipString "let"


// Expressions
let opp = new OperatorPrecedenceParser<_,_,_>()

let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, ws >>. getPosition, prec, assoc, (), (fun opPos l r -> mapping (thingy (getExprLoc l) (getExprLoc r)) l r))
    (opp.AddOperator(op))

addInfixOperator "+" 7 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Add, l, r))
addInfixOperator "-" 7 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Sub, l, r))
addInfixOperator "*" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Mul, l, r))
addInfixOperator "/" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Div, l, r))
addInfixOperator "%" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Mod, l, r))

addInfixOperator "==" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, EQ, l, r))
addInfixOperator ">" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, GT, l, r))
addInfixOperator "<" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, LT, l, r))


addInfixOperator "=" 3 Associativity.Right (fun loc l r -> UntypedExpr.Assign((), loc, l, r))

opp.AddOperator(TernaryOperator("?", ws >>. getPosition, ":", ws >>. getPosition, 10, Associativity.Left,
                                (fun condExpr thenExpr elseExpr ->
                                    let loc = (thingy (getExprLoc condExpr) (getExprLoc elseExpr))
                                    UntypedExpr.If((), loc, condExpr, thenExpr, elseExpr))))

let expr = opp.ExpressionParser

let betweenParens p =
    (between (skipChar '(') (skipChar ')') p ) .>> ws

let numericLiteral = getLoc pint64 .>> ws |>> (fun (loc, n) -> UntypedExpr.NumericLiteral ((), loc, int n))
let boolLiteral = getLoc (stringReturn "true" true <|> stringReturn "false" false) .>> ws |>> (fun (loc, b) -> UntypedExpr.BoolLiteral ((), loc, b))

// TODO: Check for keywords later on
let ident =
    getLoc (many1Satisfy isValidIdentChar .>> ws)
    >>= (fun (loc, name) ->
        if isKeyword name then
            fail $"'{name}' is a reserved keyword"
        else
            
            preturn (UntypedExpr.Ident((), loc, name))
    )

let identWithOptArgs =
    getLoc (tuple2 ident (opt (betweenParens (sepBy (ws >>. expr) (ws >>. skipChar ',')))))
    |>> (fun (loc, (id, optArgs)) ->
            match optArgs with
            | Some args -> UntypedExpr.Call((), loc, (getNameFromIdent id), args)
            | None -> UntypedExpr.Ident((), loc, (getNameFromIdent id)))

let literal =
    choice [
        numericLiteral
        boolLiteral
    ]

let typeAnnotation = choice [
    (stringReturn "int" (Type.Int))
    (stringReturn "bool" (Type.Bool))
    (stringReturn "unit" (Type.Unit))
]

let varDecl = parse{
    do! ws
    let! startPos = getPosition
    do! plet <?> "'let' keyword in variable declaration."
    
    do! ws
    let! mut = attempt pmut <?> "incomplete variable declaration."
    
    do! ws
    let! parsedIdent =  ident
    let name = getNameFromIdent parsedIdent
    
    do! ws
    let! tyAnnOpt = opt (attempt (skipChar ':') >>. ws  >>. (typeAnnotation <?> "incomplete variable declaration. Expected a type annotation."))

    do! ws
    let! initExprOpt = match mut with
                | Mutable -> opt (skipChar '=' >>. ws >>. (expr <?> "incomplete variable declaration. Expected an expression."))
                | Immutable -> (skipChar '=' >>. ws >>. expr) <?> "incomplete variable declaration. An immutable variable needs an initializer." |>> Some
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    
    do!
        if tyAnnOpt.IsNone && initExprOpt.IsNone then
            fail "incomplete variable declaration. A variable needs either a type annotation, or an initializer."
        else
            preturn ()
            
    let varExpr =
        match (tyAnnOpt, initExprOpt) with
        | (Some ty, Some initExpr) -> UntypedExpr.AnnVarDecl((), loc, name, mut, ty, Some initExpr)
        | (Some ty, None) -> UntypedExpr.AnnVarDecl((), loc, name, mut, ty, None)
        | (None, Some initExpr) -> UntypedExpr.InferredVarDecl((), loc, name, mut, None, initExpr)
        | (None, None) -> failwith "covered by the fail parser above."

    return varExpr    
}

let ifExpr =
    getLoc (tuple3 (expr .>> (ws .>> skipChar '|')) (expr .>> (ws .>> skipChar '|')) expr)
    |>> (fun (loc, (condExpr, thenExpr, elseExpr)) -> UntypedExpr.If((), loc, condExpr, thenExpr, elseExpr))

let block =
    getLoc(
        between
            (skipChar '{' .>> ws)
            (ws >>. skipChar '}' .>> ws)
            (many expr)
    )
    |>> (fun (loc, stmts) -> UntypedExpr.Block((), loc, stmts))

let func = parse {
    do! ws
    let! startPos = getPosition

    do! ws
    do! skipString "fn"

    do! ws
    let! parsedIdent = ident
    let name = getNameFromIdent parsedIdent

    do! ws
    let! paramaters =
        betweenParens
            (sepBy
                (parse {
                    do! ws
                    let! name = ident |>> getNameFromIdent
                    do! ws
                    do! skipChar ':'
                    
                    do! ws
                    let! ty = typeAnnotation
                    
                    return name, ty
                })
                (ws >>. skipChar ','))
    
    do! ws
    do! skipString "->"
    
    do! ws
    let! retType = typeAnnotation
    
    do! ws    
    let! body = block

    let! endPos = getPosition
    let loc = locFromFParsecPos startPos endPos
    
    return UntypedExpr.Func((), loc, name, paramaters, retType, body)    
}

let pwhile =
    getLoc (
        ws >>. skipString "while"
        >>. ws >>. expr
        .>>. expr
    )
    |>> (fun (loc, (cond, block)) -> UntypedExpr.While((), loc, cond, block))

let pprint = getLoc (ws >>. skipString "print" >>. ws >>. expr) |>> (fun (loc, e) -> UntypedExpr.Print((), loc, e))

opp.TermParser <- choice [
    attempt literal
    attempt block
    attempt pwhile
    attempt func
    attempt varDecl
    attempt pprint // for prototyping "parsePrint"
    attempt identWithOptArgs
    betweenParens expr
]

let program = expr .>> eof |>> CompilationUnit

let parseFile path =
    match runParserOnFile program () path System.Text.Encoding.Default with
    | Success(ast, _, _) -> Result.Ok ast
    | Failure(err, _, _) -> Result.Error err
module Koltrast.Frontend.Parser

open FParsec
open AST
open Parser_utils

WEEEEEEEEE 
type Keyword =
    | Let
    | Mut
    | While
    | Fn
    | RArrow
    | Colon
    | OpenCurly
    | ClosedCurly
    | OpenParen
    | ClosedParen
    | Comma
    | Print

let ws = spaces

let getLoc p = parse {
    do! ws
    let! startPos = getPosition
    let! pRes = p
    let! endPos = getPosition
    
    let loc = locFromFParsecPos startPos endPos
    return (loc, pRes)
}

let combineLocations loc1 loc2 = {
    StreamName = loc1.StreamName
    Start = {| Index=int loc1.Start.Index; Line=int loc1.Start.Line; Col=int loc1.Start.Col |}
    End = {| Index=int loc2.End.Index; Line=int loc2.End.Line; Col=int loc2.End.Col |}
}

let kw = function
    | Let -> skipString "let" .>> ws
    | Mut -> skipString "mut" .>> ws
    | While -> skipString "while" .>> ws
    | Fn -> skipString "fn" .>> ws
    | RArrow -> skipString "->" .>> ws
    | Colon -> skipString ":" .>> ws
    | OpenCurly -> skipString "{" .>> ws
    | ClosedCurly -> skipString "}" .>> ws
    | Comma -> skipString "," .>> ws
    | Keyword.Print -> skipString "print" .>> ws
    | OpenParen -> skipString "(" .>> ws
    | ClosedParen -> skipString ")" .>> ws

// no partial application for now.
let betweenParens p = between (kw OpenParen) (kw ClosedParen) p

let pMut = (stringReturn "mut" Mutable <|>% Immutable) .>> ws

// Operators
let opp = new OperatorPrecedenceParser<_,_,_>()

let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, ws >>. getPosition, prec, assoc, (),
        (fun opPos l r -> mapping (combineLocations (getExprLoc l) (getExprLoc r)) l r))
    (opp.AddOperator(op))

let addTernaryOperator str1 str2 prec assoc mapping =
    let op = TernaryOperator(str1, ws >>. getPosition, str2, ws >>. getPosition, prec, assoc,
        (fun condExpr thenExpr elseExpr ->
            let loc = (combineLocations (getExprLoc condExpr) (getExprLoc elseExpr))
            mapping loc condExpr thenExpr elseExpr))
    (opp.AddOperator(op))

addInfixOperator "+" 7 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Add, l, r))
addInfixOperator "-" 7 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Sub, l, r))
addInfixOperator "*" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Mul, l, r))
addInfixOperator "/" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Div, l, r))
addInfixOperator "%" 8 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Mod, l, r))

addInfixOperator "==" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Eq, l, r))
addInfixOperator ">=" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, GtEq, l, r))
addInfixOperator "<=" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, LtEq, l, r))
addInfixOperator ">" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Gt, l, r))
addInfixOperator "<" 5 Associativity.Left (fun loc l r -> UntypedExpr.BinOp((), loc, Lt, l, r))

addInfixOperator "=" 3 Associativity.Right (fun loc l r -> UntypedExpr.Assign((), loc, l, r))

addTernaryOperator "?" ":" 10 Associativity.Left (fun loc condExpr thenExpr elseExpr ->
                                    UntypedExpr.If((), loc, condExpr, thenExpr, elseExpr))

let expr = opp.ExpressionParser

let intLiteral = getLoc pint64 .>> ws |>> (fun (loc, n) -> UntypedExpr.NumericLiteral ((), loc, int n))
let boolLiteral = getLoc (stringReturn "true" true <|> stringReturn "false" false) .>> ws |>> (fun (loc, b) -> UntypedExpr.BoolLiteral ((), loc, b))

let ident =
    getLoc (many1Satisfy isValidIdentChar .>> ws) <?> "identifier"
    >>= (fun (loc, name) ->
        if isKeyword name then
            fail $"'{name}' is a reserved keyword"
        else
            preturn (UntypedExpr.Ident((), loc, name))
    )

// parse an identifier or a function call.
// Probably not the best way of doing this.
// But it works!
let identWithOptArgs =
    getLoc (tuple2 ident (opt (betweenParens (sepBy (ws >>. expr) (ws >>. skipChar ',')))))
    |>> (fun (loc, (id, optArgs)) ->
            match optArgs with
            | Some args -> UntypedExpr.Call((), loc, (getNameFromIdent id), args)
            | None -> UntypedExpr.Ident((), loc, (getNameFromIdent id)))
(**
let typeAnnotation, typeAnnotationRef = createParserForwardedToRef()

typeAnnotationRef.Value <- (
    (choice [
        (stringReturn "i64" Type.I64 .>> ws)
        (stringReturn "bool" Type.Bool .>> ws)
        (stringReturn "unit" Type.Unit .>> ws)
    ]) .>>. attempt (many (kw RArrow >>. typeAnnotation))
    .>> ws <?> "type annotation" |>> (fun (ty, tyRest) -> if tyRest.Length = 0 then ty else (Type.Fun([ty] @ tyRest[0..(tyRest.Length-1)], List.last tyRest))))
**)
let primitiveType =
    choice [
        (stringReturn "i64" Type.I64 .>> ws)
        (stringReturn "bool" Type.Bool .>> ws)
        (stringReturn "unit" Type.Unit .>> ws)
    ] .>> ws

let functype = primitiveType .>> kw RArrow .>>. primitiveType .>>. (opt (many (kw RArrow >>. primitiveType)))

let typeAnnotation =
    attempt (functype |>> (fun (simple, more) ->
        match more with
        | Some extra ->
            let entire = [fst simple] @ [snd simple] @ extra
            let parameters = List.truncate (entire.Length - 1) entire
            let retTy = List.last entire
            Type.Fun(parameters, retTy)))
    <|> primitiveType

let varDecl = parse {
    let! startPos = getPosition
    do! (kw Let) <?> "'let' keyword in variable declaration."
    
    let! mut = attempt pMut <?> "incomplete variable declaration."
    
    let! parsedIdent = ident
    let name = getNameFromIdent parsedIdent
    
    let! tyAnnOpt = opt (kw Colon >>. typeAnnotation) <?> "incomplete variable declaration. Expected a type annotation."

    let! initExprOpt =
        match mut with
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

let block =
    getLoc(
        between
            (kw OpenCurly)
            (kw ClosedCurly)
            (many expr)
    ) <?> "block"
    |>> (fun (loc, stmts) -> UntypedExpr.Block((), loc, stmts))

let func = parse {
    let! startPos = getPosition
    
    do! kw Fn

    let! parsedIdent = ident
    let name = getNameFromIdent parsedIdent
    
    let! parameters = betweenParens (sepBy ((ident |>> getNameFromIdent) .>>. (kw Colon >>. typeAnnotation)) (kw Comma))
    
    do! kw RArrow
    
    let! retType = typeAnnotation
    
    let! body = block

    let! endPos = getPosition
    let loc = locFromFParsecPos startPos endPos
    
    return UntypedExpr.Func((), loc, name, parameters, retType, body)    
}

let pwhile =
    getLoc (
        kw While
        >>. expr <?> "while condition"
        .>>. expr  <?> "while block"
    ) <?> "while loop"
    |>> (fun (loc, (cond, block)) -> UntypedExpr.While((), loc, cond, block))

let pprint = getLoc (kw Print >>. expr) |>> (fun (loc, e) -> UntypedExpr.Print((), loc, e))

opp.TermParser <- choice [
    block
    pwhile
    func
    varDecl
    pprint // for prototyping "parsePrint"
    
    intLiteral
    boolLiteral
    identWithOptArgs
    (betweenParens expr)
]

let program = block .>> eof |>> CompilationUnit

let parseFile path =
    match runParserOnFile program () path System.Text.Encoding.Default with
    | Success(ast, _, _) -> Result.Ok ast
    | Failure(err, _, _) -> Result.Error err

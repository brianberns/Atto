namespace Atto.Parser

open FParsec

type Expr =
    | If of pred : Expr * ifTrue : Expr * ifFalse : Expr
    | Equal of Expr * Expr
    | Literal of Literal
    | Name of Identifier
    | Operation of Operator * Expr * Expr
    | Call of fnName : Identifier * Expr[]

type Function =
    {
        Name : Identifier
        Args : List<Identifier>
        Expr : Expr
    }

module Parse =

    let skipToken str =
        skipString str
            .>> spaces

    let parseArgs =
        manyTill
            Identifier.parse
            (skipToken "is")

    let parseExpr, parseExprRef =
        createParserForwardedToRef ()

    let parseIf =
        pipe4
            (skipToken "if")
            parseExpr
            parseExpr
            parseExpr
            (fun _ pred ifTrue ifFalse ->
                If (pred, ifTrue, ifFalse))

    let parseEqual =
        pipe3
            (skipToken "=")
            parseExpr
            parseExpr
            (fun _ left right ->
                Equal (left, right))

    let parseLiteral =
        Literal.parse
            |>> Literal
            .>> spaces

    let parseName =
        parse {
            let! name = Identifier.parse
            let! map = getUserState
            match map |> Map.tryFind name with
                | Some nArgs ->
                    let! exprs = parray nArgs parseExpr
                    return Call (name, exprs)
                | None ->
                    return Name name
        }

    let parseOperator =
        Operator.parse
            .>> spaces

    let parseOperation =
        pipe3
            parseOperator
            parseExpr
            parseExpr
            (fun op left right ->
                Operation (op, left, right))

    let parseExprImpl =
        choice [
            parseIf
            parseEqual
            parseLiteral
            parseName
            parseOperation
        ]

    let parseFunction =
        parse {
            do! skipToken "fn"
            let! name = Identifier.parse
            let! args = parseArgs
            do! updateUserState (Map.add name args.Length)
            let! expr = parseExpr
            do! spaces
            return {
                Name = name
                Args = args
                Expr = expr
            }
        }

    let parseProgram =
        spaces
            >>. parseFunction
            .>> spaces
            .>> eof

    do parseExprRef := parseExprImpl

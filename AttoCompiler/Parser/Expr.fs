namespace Atto.Parser

open FParsec

module Reserved =

    let skip str =
        skipString str
            .>> spaces

type Expr =
    | Literal of Literal
    | Name of Identifier
    | Call of fnName : Identifier * Expr[]
    | Equal of Expr * Expr
    | Operation of Operator * Expr * Expr
    | If of pred : Expr * ifTrue : Expr * ifFalse : Expr
    | Print of Expr

module Expr =

    let private parseExpr, parseExprRef =
        createParserForwardedToRef ()

    let private parseLiteral =
        Literal.parse
            |>> Literal
            .>> spaces

    let private parseName =
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

    let private parseEqual =
        pipe3
            (Reserved.skip "=")
            parseExpr
            parseExpr
            (fun _ left right ->
                Equal (left, right))

    let private parseOperator =
        Operator.parse
            .>> spaces

    let private parseOperation =
        pipe3
            parseOperator
            parseExpr
            parseExpr
            (fun op left right ->
                Operation (op, left, right))

    let private parseIf =
        pipe4
            (Reserved.skip "if")
            parseExpr
            parseExpr
            parseExpr
            (fun _ pred ifTrue ifFalse ->
                If (pred, ifTrue, ifFalse))

    let private parsePrint =
        pipe2
            (Reserved.skip "print")
            parseExpr
            (fun _ expr -> Print expr)

    let private parseExprImpl =
        choice [
            parseIf
            parseEqual
            parseLiteral
            parsePrint
            parseName
            parseOperation
        ]

    let parse = parseExpr

    do parseExprRef := parseExprImpl

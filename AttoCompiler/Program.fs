namespace Atto

open FParsec

type Expr =
    | If of pred : Expr * ifTrue : Expr * ifFalse : Expr
    | Equal of Expr * Expr
    | Name of string
    | Number of int

type Function =
    {
        Name : string
        Args : List<string>
        Expr : Expr
    }

#if DEBUG
[<AutoOpen>]
module Debug =

    let (<!>) (p: Parser<_,_>) label : Parser<_,_> =
        fun stream ->
            printfn "%A: Entering %s" stream.Position label
            let reply = p stream
            printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
            reply
#endif

module Parse =

    let skipToken str =
        skipString str
            .>> spaces

    let parseIdentifier =
        identifier (IdentifierOptions())
            .>> spaces

    let parseArgs =
        manyTill
            parseIdentifier
            (skipToken "is")

    let parseExpr, parseExprRef = createParserForwardedToRef ()

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

    let parseName =
        parseIdentifier |>> Name

    let parseNumber =
        pint32
            .>> spaces
            |>> Number

    let parseExprImpl =
        choice [
            parseIf
            parseEqual
            parseName
            parseNumber
        ]

    let parseFunction =
        pipe4
            (skipToken "fn")
            parseIdentifier
            parseArgs
            parseExpr
            (fun _ name args expr ->
                {
                    Name = name
                    Args = args
                    Expr = expr
                })
            .>> spaces

    let parseProgram =
        spaces
            >>. parseFunction
            .>> spaces
            .>> eof

    do
        parseExprRef := parseExprImpl

module Program =

    let input =
        """
fn f n is
    if = n 0
        1
        * n f - n 1
        """.Trim()

    match runParserOnString Parse.parseProgram () "" input with
        | Success (result, _, _) -> printfn "%A" result
        | Failure (message, _, _) -> printfn "%s" message

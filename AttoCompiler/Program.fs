namespace Atto

open FParsec

type Expr =
    | If of pred : Expr * ifTrue : Expr * ifFalse : Expr

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

    let skipKeyword str =
        skipString str
            .>> spaces

    let parseName =
        identifier (IdentifierOptions())
            .>> spaces

    let parseArgs =
        manyTill
            parseName
            (skipKeyword "is")

    let parseExpr, parseExprRef = createParserForwardedToRef ()

    let parseIf =
        pipe4
            (skipKeyword "if")
            parseExpr
            parseExpr
            parseExpr
            (fun _ pred ifTrue ifFalse ->
                If (pred, ifTrue, ifFalse))

    let parseExprImpl =
        choice [
            parseIf
        ]

    let parseFunction =
        pipe4
            (skipKeyword "fn")
            parseName
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

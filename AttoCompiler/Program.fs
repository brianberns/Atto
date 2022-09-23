namespace Atto

open FParsec

type Literal =
    | Num of float
    | Str of string

module Literal =

    let parseString =
        between
            (pchar '"')
            (pchar '"')
            (manySatisfy ((<) '"'))

    let parse =
        choice [
            parseString |>> Str
            pfloat |>> Num
        ]

type Operator =
    | Addition
    | Subtraction
    | Multiplication

module Operator =

    let parse =
        choice [
            skipChar '+' >>% Addition
            skipChar '-' >>% Subtraction
            skipChar '*' >>% Multiplication
        ]

module Identifier =

    let parse =
        identifier (IdentifierOptions())
            .>> spaces

type Expr =
    | If of pred : Expr * ifTrue : Expr * ifFalse : Expr
    | Equal of Expr * Expr
    | Literal of Literal
    | Name of string
    | Operation of Operator * Expr * Expr
    | Call of fnName : string * Expr[]

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
            let sResult = string reply.Result
            printfn "%A: Leaving %s (%A): %s" stream.Position label reply.Status sResult
            reply
#endif

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

module Program =

    let input =
        """
fn f n is
    if = n 0
        1
        * n f - n 1
        """.Trim()

    match runParserOnString Parse.parseProgram Map.empty "" input with
        | Success (result, _, _) -> printfn "%A" result
        | Failure (message, _, _) -> printfn "%s" message

namespace Atto.Parser

open FParsec

type Function =
    {
        Name : Identifier
        Args : List<Identifier>
        Expr : Expr
    }

module Function =

    let private parseArgs =
        manyTill
            Identifier.parse
            (Reserved.skip "is")

    let parse =
        parse {
            do! Reserved.skip "fn"
            let! name = Identifier.parse
            let! args = parseArgs
            do! updateUserState (Map.add name args.Length)
            let! expr = Expr.parse
            do! spaces
            return {
                Name = name
                Args = args
                Expr = expr
            }
        }

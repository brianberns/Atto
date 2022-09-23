namespace Atto.Parser

open FParsec

module Program =

    let parse =
        spaces
            >>. Function.parse
            .>> spaces
            .>> eof

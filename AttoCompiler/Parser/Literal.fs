namespace Atto.Parser

open FParsec

type Literal =

    /// E.g. 123.45.
    | Num of float

    /// E.g. "my string".
    | Str of string

module Literal =

    let private parseString<'s> : Parser<_, 's> =
        between
            (pchar '"')
            (pchar '"')
            (manySatisfy ((<) '"'))

    let parse<'s> : Parser<_, 's> =
        choice [
            parseString |>> Str
            pfloat |>> Num
        ]

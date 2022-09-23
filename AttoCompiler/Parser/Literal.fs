namespace Atto.Parser

open FParsec

type Literal =
    | Num of float
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

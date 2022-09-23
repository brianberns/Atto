namespace Atto.Parser

open FParsec

type Identifier = Identifier of string

module Identifier =

    let parse<'s> : Parser<_, 's> =
        identifier (IdentifierOptions())
            |>> Identifier
            .>> spaces

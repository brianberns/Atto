namespace Atto.Parser

open FParsec

type Identifier = Identifier of string
    with
    member this.String =
        let (Identifier str) = this in str

module Identifier =

    /// Parses an identifier and skips any trailing spaces.
    let parse<'s> : Parser<_, 's> =
        identifier (IdentifierOptions())
            |>> Identifier
            .>> spaces

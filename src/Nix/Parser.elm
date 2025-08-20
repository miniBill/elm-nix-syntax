module Nix.Parser exposing (DeadEnd, Parser, parse)

import Nix.Parser.Context exposing (Context)
import Nix.Parser.Internal
import Nix.Parser.Problem exposing (Problem(..))
import Nix.Syntax.Expression exposing (Expression)
import Nix.Syntax.Node exposing (Node)
import Parser.Advanced as Parser exposing ((|.), (|=), end, succeed)


type alias Parser a =
    Parser.Parser Context Problem a


type alias DeadEnd =
    Parser.DeadEnd Context Problem


parse : String -> Result (List DeadEnd) (Node Expression)
parse input =
    Parser.run
        (succeed identity
            |. Nix.Parser.Internal.spaces
            |= Nix.Parser.Internal.expression
            |. end ExpectingEnd
        )
        input

module Utils exposing (emptyLocation, emptyRange, node)

import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location, Range)


node : a -> Node a
node x =
    Node emptyRange x


emptyRange : Range
emptyRange =
    { start = emptyLocation
    , end = emptyLocation
    }


emptyLocation : Location
emptyLocation =
    { row = 0
    , column = 0
    }

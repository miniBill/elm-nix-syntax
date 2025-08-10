module Nix.Syntax.Range exposing (Location, Range)


type alias Range =
    { start : Location
    , end : Location
    }


type alias Location =
    { row : Int
    , column : Int
    }

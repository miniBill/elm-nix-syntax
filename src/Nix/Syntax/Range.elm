module Nix.Syntax.Range exposing (Location, Range, empty)

{-|

@docs Location, Range, empty

-}


{-| Source location
-}
type alias Location =
    { row : Int
    , column : Int
    }


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }


{-| Construct an empty range
-}
empty : Range
empty =
    { start = { row = 0, column = 0 }
    , end = { row = 0, column = 0 }
    }

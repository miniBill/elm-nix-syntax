module Nix.Parser.Problem exposing (Problem(..))

{-|

@docs Problem

-}


type Problem
    = ExpectingEnd
    | Expecting String
    | ExpectingVariable
    | ExpectingIdentifier
    | ExpectingDigit
    | Unexpected String
    | Unimplemented String

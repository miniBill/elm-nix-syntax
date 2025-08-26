module Nix.Parser.Problem exposing (Problem(..))


type Problem
    = ExpectingEnd
    | Expecting String
    | ExpectingVariable
    | ExpectingIdentifier
    | ExpectingDigit
    | Unexpected String
    | Unimplemented String

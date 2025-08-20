module Nix.Parser.Problem exposing (Problem(..))


type Problem
    = ExpectingEnd
    | Expecting String
    | ExpectingVariable
    | ExpectingDigit
    | Unexpected String
    | Unimplemented String

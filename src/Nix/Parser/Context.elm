module Nix.Parser.Context exposing (Context(..))

{-|

@docs Context

-}


type Context
    = ParsingExpression
    | ParsingString
    | ParsingList
    | ParsingAttrset
    | ParsingFunction
    | ParsingLet
    | ParsingApplication
    | ParsingPattern
    | ParsingIfThenElse

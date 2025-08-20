module Nix.Parser.Context exposing (Context(..))


type Context
    = ParsingExpression
    | ParsingString
    | ParsingList
    | ParsingAttrset
    | ParsingFunction
    | ParsingLet
    | ParsingApplication
    | ParsingPattern

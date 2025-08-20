module Nix.Syntax.Node exposing (Node(..), range, value)

import Nix.Syntax.Range exposing (Range)


type Node a
    = Node Range a


value : Node a -> a
value (Node _ v) =
    v


range : Node a -> Range
range (Node r _) =
    r

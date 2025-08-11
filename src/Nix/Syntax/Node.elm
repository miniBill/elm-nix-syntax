module Nix.Syntax.Node exposing (Node(..), value)

import Nix.Syntax.Range exposing (Range)


type Node a
    = Node Range a


value : Node a -> a
value (Node _ v) =
    v

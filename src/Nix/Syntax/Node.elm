module Nix.Syntax.Node exposing (Node(..))

import Nix.Syntax.Range exposing (Range)


type Node a
    = Node Range a

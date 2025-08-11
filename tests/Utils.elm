module Utils exposing (checkParser, emptyLocation, emptyRange, node)

import Expect
import ExpectEqual
import Nix.Parser
import Nix.Syntax.Expression exposing (Expression)
import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location, Range)


node : a -> Node a
node x =
    Node emptyRange x


emptyRange : Range
emptyRange =
    { start = emptyLocation
    , end = emptyLocation
    }


emptyLocation : Location
emptyLocation =
    { row = 0
    , column = 0
    }


checkParser :
    String
    -> Node Expression
    -> Expect.Expectation
checkParser input value =
    case Nix.Parser.parse input of
        Ok parsed ->
            parsed
                |> ExpectEqual.nodeExpression value

        Err e ->
            Expect.fail (Nix.Parser.errorToString input e)

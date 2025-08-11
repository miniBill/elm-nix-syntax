module Utils exposing (checkParser, node)

import Expect
import Fake
import Nix.Parser
import Nix.Syntax.Expression exposing (Expression(..))
import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location, Range)


node : a -> Node a
node x =
    Node Fake.emptyRange x


checkParser :
    String
    -> Node Expression
    -> Expect.Expectation
checkParser input value =
    case Nix.Parser.parse input of
        Ok parsed ->
            parsed
                |> Fake.nodeExpression
                |> Expect.equal value

        Err e ->
            Expect.fail (Nix.Parser.errorToString input e)

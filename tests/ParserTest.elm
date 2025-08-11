module ParserTest exposing (suite)

import Nix.Syntax.Expression exposing (Expression(..))
import Test exposing (Test)
import Utils exposing (node)


suite : Test
suite =
    Test.test "Function application" <|
        \_ ->
            Utils.checkParser "a b"
                (node
                    (ApplicationExpr
                        (node (VariableExpr "a"))
                        [ node (VariableExpr "b") ]
                    )
                )

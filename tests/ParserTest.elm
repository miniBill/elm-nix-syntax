module ParserTest exposing (functionApplication, lambda, recordPattern, recordPattern2)

import Nix.Syntax.Expression exposing (Expression(..), Pattern(..), RecordFieldPattern(..))
import Test exposing (Test)
import Utils exposing (node, string, var)


functionApplication : Test
functionApplication =
    Test.test "Function application" <|
        \_ ->
            Utils.checkParser "a b"
                (node
                    (ApplicationExpr
                        (node (VariableExpr "a"))
                        [ node (VariableExpr "b") ]
                    )
                )


lambda : Test
lambda =
    Test.test "Lambda" <|
        \_ ->
            Utils.checkParser "{ lib }: lib"
                (node
                    (FunctionExpr
                        (node
                            (RecordPattern
                                [ RecordFieldPattern (node "lib") Nothing ]
                                { open = False }
                            )
                        )
                        (var "lib")
                    )
                )


recordPattern : Test
recordPattern =
    Test.test "Record pattern" <|
        \_ ->
            Utils.checkPatternParser "{ lib }"
                (node
                    (RecordPattern
                        [ RecordFieldPattern (node "lib") Nothing ]
                        { open = False }
                    )
                )


recordPattern2 : Test
recordPattern2 =
    Test.test "Record pattern 2" <|
        \_ ->
            Utils.checkPatternParser """{ system, username ? "minibill", module, }"""
                (node
                    (RecordPattern
                        [ RecordFieldPattern (node "system") Nothing
                        , RecordFieldPattern (node "username") (Just (string "minibill"))
                        , RecordFieldPattern (node "module") Nothing
                        ]
                        { open = False }
                    )
                )

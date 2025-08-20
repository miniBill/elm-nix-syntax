module ParserTest exposing (commentTest, functionApplication, lambda, multilineStringTest, recordPattern, recordPattern2, stringTest)

import Nix.Syntax.Expression exposing (Expression(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node exposing (Node)
import Test exposing (Test)
import Utils exposing (int, node, string, var)


test : String -> String -> Node Expression -> Test
test label input output =
    Test.test label <|
        \_ ->
            Utils.checkParser input output


stringTest : Test
stringTest =
    test "String" "\"hello world\"" (string "hello world")


multilineStringTest : Test
multilineStringTest =
    test "Indented string"
        """
            ''
                multi
                 line
                  string 
            ''
        """
        (node
            (StringExpr [ StringLiteral "multi\n line\n  string " ])
        )


commentTest : Test
commentTest =
    test "Comment"
        """
            # Comment
            0
        """
        (int 0)


functionApplication : Test
functionApplication =
    test "Function application"
        "a b"
        (node
            (ApplicationExpr
                (node (VariableExpr "a"))
                [ node (VariableExpr "b") ]
            )
        )


lambda : Test
lambda =
    test "Lambda"
        "{ lib }: lib"
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

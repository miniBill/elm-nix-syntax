module ParserTest exposing (addition, booleansFalseTest, booleansTrueTest, commentTest, floatTest, functionApplication, intTest, interpolatedAccess, lambda, longPathTest, multilineStringTest, nullTest, recordPattern, recordPattern2, stringInterpolationTest, stringInterpolationTest2, stringTest)

import Nix.Syntax.Expression exposing (Expression(..), Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node exposing (Node)
import Test exposing (Test)
import Utils exposing (apply, bool, float, int, minus, node, null, plus, record, string, var)


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


stringInterpolationTest : Test
stringInterpolationTest =
    test "String interpolation"
        "\"hello ${ { a = \"world\"; }.a }\""
        (node
            (StringExpr
                [ StringLiteral "hello "
                , StringInterpolation
                    (node
                        (AttributeSelectionExpr
                            (record [ ( [ "a" ], string "world" ) ])
                            [ node (IdentifierName "a") ]
                            Nothing
                        )
                    )
                ]
            )
        )


stringInterpolationTest2 : Test
stringInterpolationTest2 =
    test "String interpolation #2"
        "\"1 2 ${toString 3}\""
        (node
            (StringExpr
                [ StringLiteral "1 2 "
                , StringInterpolation (apply (var "toString") [ int 3 ])
                ]
            )
        )


booleansTrueTest : Test
booleansTrueTest =
    test "Booleans (true)"
        "true"
        (bool True)


booleansFalseTest : Test
booleansFalseTest =
    test "Booleans (false)"
        "false"
        (bool False)


nullTest : Test
nullTest =
    test "Null"
        "null"
        null


intTest : Test
intTest =
    test "Int"
        "123"
        (int 123)


floatTest : Test
floatTest =
    test "Float"
        "123.456"
        (float 123.456)


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


addition : Test
addition =
    test "Addition"
        "2 + 3 - 4 + 1"
        (plus (minus (plus (int 2) (int 3)) (int 4)) (int 1))


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


longPathTest : Test
longPathTest =
    Test.test "Long path" <|
        \_ ->
            Utils.checkPathParser
                "../../programs/wally-cli.nix"
                [ [ StringLiteral ".." ]
                , [ StringLiteral ".." ]
                , [ StringLiteral "programs" ]
                , [ StringLiteral "wally-cli.nix" ]
                ]


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


interpolatedAccess : Test
interpolatedAccess =
    test "Interpolated path access"
        """
        {}
        .${l}
        """
        (node
            (AttributeSelectionExpr
                (record [])
                [ node (InterpolationName (var "l")) ]
                Nothing
            )
        )

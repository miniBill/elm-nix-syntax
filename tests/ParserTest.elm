module ParserTest exposing (addition, booleansFalseTest, booleansTrueTest, commentTest, floatTest, functionApplication, identifier, idris, intTest, interpolatedAccess, lambda, longPathTest, multilineStringTest, nullTest, recordPattern, recordPattern2, stringInterpolationTest, stringInterpolationTest2, stringInterpolationTest3, stringTest, weirdPatter)

import Nix.Syntax.Expression exposing (Expression(..), Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node as Node exposing (Node)
import Test exposing (Test)
import Utils exposing (apply, bool, float, int, let_, list, minus, null, path, plus, record, string, var)


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
        (Node.empty
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
        (Node.empty
            (StringExpr
                [ StringLiteral "hello "
                , StringInterpolation
                    (Node.empty
                        (AttributeSelectionExpr
                            (record [ ( [ "a" ], string "world" ) ])
                            [ Node.empty (IdentifierName "a") ]
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
        (Node.empty
            (StringExpr
                [ StringLiteral "1 2 "
                , StringInterpolation (apply (var "toString") [ int 3 ])
                ]
            )
        )


stringInterpolationTest3 : Test
stringInterpolationTest3 =
    test "String interpolation #3"
        "\"${toString ./..}/.git-revision\""
        (Node.empty
            (StringExpr
                [ StringInterpolation
                    (apply
                        (var "toString")
                        [ path [ ".", ".." ] ]
                    )
                , StringLiteral "/.git-revision"
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
        (Node.empty
            (ApplicationExpr
                (Node.empty (VariableExpr "a"))
                [ Node.empty (VariableExpr "b") ]
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
        (Node.empty
            (FunctionExpr
                (Node.empty
                    (RecordPattern
                        [ RecordFieldPattern (Node.empty "lib") Nothing ]
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
                (Node.empty
                    (RecordPattern
                        [ RecordFieldPattern (Node.empty "lib") Nothing ]
                        { open = False }
                    )
                )


recordPattern2 : Test
recordPattern2 =
    Test.test "Record pattern 2" <|
        \_ ->
            Utils.checkPatternParser """{ system, username ? "minibill", module, }"""
                (Node.empty
                    (RecordPattern
                        [ RecordFieldPattern (Node.empty "system") Nothing
                        , RecordFieldPattern (Node.empty "username") (Just (string "minibill"))
                        , RecordFieldPattern (Node.empty "module") Nothing
                        ]
                        { open = False }
                    )
                )


weirdPatter : Test
weirdPatter =
    Test.test "Weird pattern" <|
        \_ ->
            Utils.checkPatternParser
                """{
                    lib ? import ../..,
                    modules ? [ ],
                }"""
                (Node.empty
                    (RecordPattern
                        [ RecordFieldPattern (Node.empty "lib")
                            (Just (apply (var "import") [ path [ "..", ".." ] ]))
                        , RecordFieldPattern (Node.empty "modules")
                            (Just (list []))
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
        (Node.empty
            (AttributeSelectionExpr
                (record [])
                [ Node.empty (InterpolationName (var "l")) ]
                Nothing
            )
        )


idris : Test
idris =
    test "???"
        """
        let
            idris-with-packages = with-packages;
            newAttrs = [  ];
        in
        null """
        (let_
            [ ( "idris-with-packages", var "with-packages" )
            , ( "newAttrs", list [] )
            ]
            null
        )


identifier : Test
identifier =
    test "identifier can start with a reserved keyword"
        "with-packages"
        (var "with-packages")

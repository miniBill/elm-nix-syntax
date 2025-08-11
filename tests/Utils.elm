module Utils exposing (checkParser, key, node, record, string)

import Ansi.Color
import Diff
import Diff.ToString
import Expect
import Fake
import Nix.Parser
import Nix.Syntax.Expression exposing (AttrPath, Expression(..), Name(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))


record : List ( List String, Node Expression ) -> Node Expression
record attrs =
    node
        (RecordExpr
            (List.map
                (\( k, v ) ->
                    node
                        ( key k
                        , v
                        )
                )
                attrs
            )
        )


key : List String -> Node AttrPath
key parts =
    node (List.map (\part -> node (IdentifierName part)) parts)


string : String -> Node Expression
string v =
    node (StringExpr [ StringLiteral v ])


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
                |> toStringish
                |> expectEqualMultiline (toStringish value)

        Err e ->
            Expect.fail (Nix.Parser.errorToString input e)


toStringish : a -> String
toStringish v =
    Debug.toString v
        |> String.replace "Node" "\nNode"
        |> String.trim


expectEqualMultiline : String -> String -> Expect.Expectation
expectEqualMultiline exp actual =
    if exp == actual then
        Expect.pass

    else
        let
            header : String
            header =
                Ansi.Color.fontColor Ansi.Color.blue "Diff from expected to actual:"
        in
        Expect.fail
            (header
                ++ "\n"
                ++ (Diff.diffLinesWith
                        (Diff.defaultOptions
                            |> Diff.ignoreLeadingWhitespace
                        )
                        exp
                        actual
                        |> Diff.ToString.diffToString { context = 4, color = True }
                   )
            )

module Utils exposing (apply, checkParser, dot, function, key, let_, list, node, parens, record, string, var)

import Ansi.Color
import Diff
import Diff.ToString
import Expect
import Fake
import Nix.Parser
import Nix.Syntax.Expression exposing (AttrPath, Expression(..), Name(..), Pattern(..), StringElement(..))
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


list : List (Node Expression) -> Node Expression
list cs =
    node (ListExpr cs)


function : String -> Node Expression -> Node Expression
function v e =
    node (FunctionExpr (node (VarPattern v)) e)


apply : Node Expression -> List (Node Expression) -> Node Expression
apply v cs =
    node (ApplicationExpr v cs)


var : String -> Node Expression
var v =
    node (VariableExpr v)


parens : Node Expression -> Node Expression
parens v =
    node (ParenthesizedExpression v)


dot : Node Expression -> List String -> Node Expression
dot e k =
    node (AttributeSelection e (List.map node k) Nothing)


let_ :
    List ( String, Node Expression )
    -> Node Expression
    -> Node Expression
let_ cs e =
    node
        (LetExpression
            (List.map
                (\( k, v ) -> node ( node k, v ))
                cs
            )
            e
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

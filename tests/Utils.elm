module Utils exposing (apply, attribute, bool, checkParser, checkPathParser, checkPatternParser, dot, function, int, let_, list, node, null, parens, record, string, update, var)

import Ansi.Color
import Diff
import Diff.ToString
import Expect
import Fake
import Nix.Parser
import Nix.Parser.Extra
import Nix.Parser.Internal
import Nix.Parser.Problem
import Nix.Syntax.Expression exposing (AttrPath, Attribute(..), Expression(..), LetDeclaration(..), Name(..), Path, Pattern(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))
import Parser.Advanced as Parser exposing ((|.), (|=))


record : List ( List String, Node Expression ) -> Node Expression
record attrs =
    node
        (RecordExpr
            (List.map
                (\( k, v ) ->
                    attribute k v
                )
                attrs
            )
        )


attribute : List String -> Node Expression -> Node Attribute
attribute k v =
    node
        (Attribute
            (key k)
            v
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


update : Node Expression -> Node Expression -> Node Expression
update l r =
    node (OperatorApplicationExpr l (node "//") r)


parens : Node Expression -> Node Expression
parens v =
    node (ParenthesizedExpr v)


dot : Node Expression -> List String -> Node Expression
dot e k =
    node (AttributeSelectionExpr e (List.map node k) Nothing)


let_ :
    List ( String, Node Expression )
    -> Node Expression
    -> Node Expression
let_ cs e =
    node
        (LetExpr
            (List.map
                (\( k, v ) -> node (LetDeclaration (node k) v))
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


int : Int -> Node Expression
int v =
    node (IntExpr v)


bool : Bool -> Node Expression
bool v =
    node (BoolExpr v)


null : Node Expression
null =
    node NullExpr


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
            Expect.fail (Nix.Parser.Extra.errorToString input e)


checkPathParser :
    String
    -> Path
    -> Expect.Expectation
checkPathParser input value =
    case
        Parser.run
            (Parser.succeed identity
                |= Nix.Parser.Internal.path
                |. Parser.end Nix.Parser.Problem.ExpectingEnd
            )
            input
    of
        Ok parsed ->
            parsed
                |> toStringish
                |> expectEqualMultiline (toStringish value)

        Err e ->
            Expect.fail (Nix.Parser.Extra.errorToString input e)


checkPatternParser :
    String
    -> Node Pattern
    -> Expect.Expectation
checkPatternParser input value =
    case
        Parser.run
            (Parser.succeed identity
                |= Nix.Parser.Internal.pattern
                |. Parser.end Nix.Parser.Problem.ExpectingEnd
            )
            input
    of
        Ok parsed ->
            parsed
                |> Fake.nodePattern
                |> toStringish
                |> expectEqualMultiline (toStringish value)

        Err e ->
            Expect.fail (Nix.Parser.Extra.errorToString input e)


toStringish : a -> String
toStringish v =
    Debug.toString v
        |> String.replace " { end = { column = 0, row = 0 }, start = { column = 0, row = 0 } }" ""
        |> String.replace "Node" "\nNode"
        |> String.split "\n"
        |> List.foldl
            (\e ( a, ai ) ->
                let
                    nai : Float
                    nai =
                        if String.contains "Node [" e || String.contains "Node (" e then
                            ai + 0.25

                        else if String.contains "Node ]" e || String.contains "Node )" e then
                            ai - 0.25

                        else
                            ai
                in
                ( (String.repeat (floor ai - 16) " " ++ e) :: a, nai )
            )
            ( [], 0 )
        |> Tuple.first
        |> List.reverse
        |> String.join "\n"
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

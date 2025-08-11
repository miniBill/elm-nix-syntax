module ExpectEqual exposing (nodeExpression)

import Expect exposing (Expectation)
import Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), Name(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))


nodeExpression : Node Expression -> Node Expression -> Expectation
nodeExpression e a =
    node expression e a


node : (a -> a -> Expectation) -> Node a -> Node a -> Expectation
node inner (Node _ e) (Node _ a) =
    inner e a


list :
    (a -> a -> Expectation)
    -> List a
    -> List a
    -> Expectation
list inner e a =
    case ( e, a ) of
        ( [], [] ) ->
            Expect.pass

        ( eh :: et, ah :: at ) ->
            Expect.all
                [ \_ -> inner eh ah
                , \_ -> list inner et at
                ]
                ()

        _ ->
            Expect.equal e a


expression : Expression -> Expression -> Expectation
expression e a =
    case ( e, a ) of
        ( ApplicationExpr eh et, ApplicationExpr ah at ) ->
            list nodeExpression (eh :: et) (ah :: at)

        ( VariableExpr ev, VariableExpr av ) ->
            Expect.equal ev av

        ( StringExpr ev, StringExpr av ) ->
            list stringElement ev av

        ( LetExpression _ _, _ ) ->
            Debug.todo "branch '( LetExpression _ _, _ )' not implemented"

        ( FunctionExpr _ _, _ ) ->
            Debug.todo "branch '( FunctionExpr _ _, _ )' not implemented"

        ( RecordExpr ev, RecordExpr av ) ->
            list (node attribute) ev av

        ( ListExpr ev, ListExpr av ) ->
            list nodeExpression ev av

        _ ->
            Expect.equal e a


attribute : Attribute -> Attribute -> Expectation
attribute e a =
    tuple (node attrPath) nodeExpression e a


attrPath : AttrPath -> AttrPath -> Expectation
attrPath e a =
    list (node name) e a


name : Name -> Name -> Expectation
name e a =
    case ( e, a ) of
        ( IdentifierName ev, IdentifierName av ) ->
            Expect.equal ev av

        ( StringName ev, StringName av ) ->
            list stringElement ev av

        _ ->
            Expect.equal e a


tuple :
    (a -> a -> Expectation)
    -> (b -> b -> Expectation)
    -> ( a, b )
    -> ( a, b )
    -> Expectation
tuple l r ( el, er ) ( al, ar ) =
    Expect.all
        [ \_ -> l el al
        , \_ -> r er ar
        ]
        ()


stringElement : StringElement -> StringElement -> Expectation
stringElement e a =
    case ( e, a ) of
        ( StringInterpolation ev, StringInterpolation av ) ->
            nodeExpression ev av

        _ ->
            Expect.equal e a

module ExpectEqual exposing (nodeExpression)

import Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), Name(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))


nodeExpression : Node Expression -> Node Expression -> Bool
nodeExpression e a =
    node expression e a


node : (a -> a -> Bool) -> Node a -> Node a -> Bool
node inner (Node _ e) (Node _ a) =
    inner e a


list :
    (a -> a -> Bool)
    -> List a
    -> List a
    -> Bool
list inner e a =
    case ( e, a ) of
        ( [], [] ) ->
            True

        ( eh :: et, ah :: at ) ->
            if inner eh ah then
                list inner et at

            else
                False

        _ ->
            False


expression : Expression -> Expression -> Bool
expression e a =
    case ( e, a ) of
        ( ApplicationExpr eh et, ApplicationExpr ah at ) ->
            list nodeExpression (eh :: et) (ah :: at)

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
            e == a


attribute : Attribute -> Attribute -> Bool
attribute e a =
    tuple (node attrPath) nodeExpression e a


attrPath : AttrPath -> AttrPath -> Bool
attrPath e a =
    list (node name) e a


name : Name -> Name -> Bool
name e a =
    case ( e, a ) of
        ( StringName ev, StringName av ) ->
            list stringElement ev av

        _ ->
            e == a


tuple :
    (a -> a -> Bool)
    -> (b -> b -> Bool)
    -> ( a, b )
    -> ( a, b )
    -> Bool
tuple l r ( el, er ) ( al, ar ) =
    l el al && r er ar


stringElement : StringElement -> StringElement -> Bool
stringElement e a =
    case ( e, a ) of
        ( StringInterpolation ev, StringInterpolation av ) ->
            nodeExpression ev av

        _ ->
            e == a

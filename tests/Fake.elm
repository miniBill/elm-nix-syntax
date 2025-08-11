module Fake exposing (..)

import Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), LetDeclaration, Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location, Range)


nodeExpression : Node Expression -> Node Expression
nodeExpression e =
    node expression e


expression : Expression -> Expression
expression e =
    case e of
        ApplicationExpr c cs ->
            ApplicationExpr (nodeExpression c) (List.map nodeExpression cs)

        Negation c ->
            Negation (nodeExpression c)

        StringExpr cs ->
            StringExpr (List.map stringElement cs)

        ParenthesizedExpression c ->
            ParenthesizedExpression (nodeExpression c)

        LetExpression ds c ->
            LetExpression (List.map (node letDeclaration) ds) (nodeExpression c)

        AttributeSelection v a d ->
            AttributeSelection
                (nodeExpression v)
                (List.map (node identityString) a)
                (Maybe.map nodeExpression d)

        UpdateExpression l r ->
            UpdateExpression (nodeExpression l) (nodeExpression r)

        FunctionExpr p c ->
            FunctionExpr (node pattern p) (nodeExpression c)

        RecordExpr attrs ->
            RecordExpr (List.map (node attribute) attrs)

        ListExpr cs ->
            ListExpr (List.map nodeExpression cs)

        PathExpr p ->
            PathExpr (List.map identityString p)

        VariableExpr v ->
            VariableExpr (identityString v)


letDeclaration : LetDeclaration -> LetDeclaration
letDeclaration ( k, v ) =
    ( node identityString k, nodeExpression v )


pattern : Pattern -> Pattern
pattern p =
    case p of
        RecordPattern cs open ->
            RecordPattern (List.map recordFieldPattern cs) open

        AllPattern ->
            p

        VarPattern v ->
            VarPattern (identityString v)

        ParenthesizedPattern c ->
            ParenthesizedPattern (node pattern c)


recordFieldPattern : RecordFieldPattern -> RecordFieldPattern
recordFieldPattern (RecordFieldPattern k default) =
    RecordFieldPattern
        (node identityString k)
        (Maybe.map nodeExpression default)


{-| Identity, but forced to be on Strings
-}
identityString : String -> String
identityString a =
    a


attribute : Attribute -> Attribute
attribute ( k, v ) =
    ( node attrpath k, node expression v )


attrpath : AttrPath -> AttrPath
attrpath segments =
    List.map (node name) segments


name : Name -> Name
name v =
    case v of
        IdentifierName c ->
            IdentifierName (identityString c)

        StringName es ->
            StringName (List.map stringElement es)


stringElement : StringElement -> StringElement
stringElement e =
    case e of
        StringLiteral c ->
            StringLiteral (identityString c)

        StringInterpolation c ->
            StringInterpolation (nodeExpression c)


node : (a -> a) -> Node a -> Node a
node inner (Node _ x) =
    Node emptyRange (inner x)


emptyRange : Range
emptyRange =
    { start = emptyLocation
    , end = emptyLocation
    }


emptyLocation : Location
emptyLocation =
    { row = 0
    , column = 0
    }

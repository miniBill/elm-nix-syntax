module Fake exposing (emptyRange, nodeExpression, nodePattern)

import Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), LetDeclaration(..), Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location, Range)


nodeExpression : Node Expression -> Node Expression
nodeExpression e =
    node expression e


nodePattern : Node Pattern -> Node Pattern
nodePattern p =
    node pattern p


expression : Expression -> Expression
expression e =
    case e of
        ApplicationExpr c cs ->
            ApplicationExpr (nodeExpression c) (List.map nodeExpression cs)

        NegationExpr c ->
            NegationExpr (nodeExpression c)

        StringExpr cs ->
            StringExpr (List.map stringElement cs)

        ParenthesizedExpr c ->
            ParenthesizedExpr (nodeExpression c)

        LetExpr ds c ->
            LetExpr (List.map (node letDeclaration) ds) (nodeExpression c)

        AttributeSelectionExpr v a d ->
            AttributeSelectionExpr
                (nodeExpression v)
                (List.map (node identityString) a)
                (Maybe.map nodeExpression d)

        FunctionExpr p c ->
            FunctionExpr (nodePattern p) (nodeExpression c)

        RecordExpr attrs ->
            RecordExpr (List.map (node attribute) attrs)

        ListExpr cs ->
            ListExpr (List.map nodeExpression cs)

        PathExpr p ->
            PathExpr (List.map identityString p)

        VariableExpr v ->
            VariableExpr (identityString v)

        IntExpr i ->
            IntExpr (identityNumber i)

        FloatExpr f ->
            FloatExpr (identityNumber f)

        BoolExpr b ->
            BoolExpr (identityBool b)

        NullExpr ->
            NullExpr

        WithExpr l r ->
            WithExpr (nodeExpression l) (nodeExpression r)

        OperatorApplicationExpr l op r ->
            OperatorApplicationExpr (nodeExpression l) (node identityString op) (nodeExpression r)


identityBool : Bool -> Bool
identityBool v =
    v


identityNumber : number -> number
identityNumber n =
    n


letDeclaration : LetDeclaration -> LetDeclaration
letDeclaration decl =
    case decl of
        LetDeclaration k v ->
            LetDeclaration (node identityString k) (nodeExpression v)

        LetInheritVariables vs ->
            LetInheritVariables (List.map (node identityString) vs)

        LetInheritFromSet s vs ->
            LetInheritFromSet (node identityString s) (List.map (node identityString) vs)


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
            ParenthesizedPattern (nodePattern c)


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

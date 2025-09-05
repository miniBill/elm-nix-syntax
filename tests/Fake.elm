module Fake exposing (nodeExpression, nodePattern)

import Nix.Syntax.Expression exposing (AttrPath, Attribute(..), Expression(..), LetDeclaration(..), Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))
import Nix.Syntax.Node as Node exposing (Node(..))


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

        DotExpr v a d ->
            DotExpr
                (nodeExpression v)
                (List.map (node name) a)
                (Maybe.map nodeExpression d)

        FunctionExpr p c ->
            FunctionExpr (nodePattern p) (nodeExpression c)

        AttrSetExpr attrs ->
            AttrSetExpr (List.map (node attribute) attrs)

        ListExpr cs ->
            ListExpr (List.map nodeExpression cs)

        PathExpr p ->
            PathExpr (List.map (List.map stringElement) p)

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

        HasAttributeExpr l r ->
            HasAttributeExpr (nodeExpression l) (node attrpath r)

        AssertExpr l r ->
            AssertExpr (nodeExpression l) (nodeExpression r)

        OperatorApplicationExpr l op r ->
            OperatorApplicationExpr (nodeExpression l) (node identityString op) (nodeExpression r)

        LookupPathExpr c ->
            LookupPathExpr (List.map identityString c)

        IfExpr c t f ->
            IfExpr (nodeExpression c) (nodeExpression t) (nodeExpression f)


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
            LetDeclaration (node attrpath k) (nodeExpression v)

        LetInheritVariables vs ->
            LetInheritVariables (List.map (node identityString) vs)

        LetInheritFromAttrSet s vs ->
            LetInheritFromAttrSet (nodeExpression s) (List.map (node identityString) vs)


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

        AtPattern c n ->
            AtPattern (nodePattern c) (node identityString n)

        ReverseAtPattern n c ->
            ReverseAtPattern (node identityString n) (nodePattern c)


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
attribute attr =
    case attr of
        Attribute k v ->
            Attribute (node attrpath k) (node expression v)

        AttributeInheritVariables vs ->
            AttributeInheritVariables (List.map (node identityString) vs)

        AttributeInheritFromAttrSet s vs ->
            AttributeInheritFromAttrSet (nodeExpression s) (List.map (node identityString) vs)


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

        InterpolationName e ->
            InterpolationName (nodeExpression e)


stringElement : StringElement -> StringElement
stringElement e =
    case e of
        StringLiteral c ->
            StringLiteral (identityString c)

        StringInterpolation c ->
            StringInterpolation (nodeExpression c)


node : (a -> a) -> Node a -> Node a
node inner (Node _ x) =
    Node.empty (inner x)

module Nix.Syntax.Expression exposing (AttrPath, Attribute(..), Expression(..), LetDeclaration(..), Name(..), Path, Pattern(..), RecordFieldPattern(..), StringElement(..))

{-|

@docs AttrPath, Attribute, Expression, LetDeclaration, Name, Path, Pattern, RecordFieldPattern, StringElement

-}

import Nix.Syntax.Node exposing (Node)


type Expression
    = NullExpr
    | ApplicationExpr (Node Expression) (List (Node Expression))
    | OperatorApplicationExpr (Node Expression) (Node String) (Node Expression)
    | VariableExpr String
    | IfThenElseExpr (Node Expression) (Node Expression) (Node Expression)
    | BoolExpr Bool
    | IntExpr Int
    | FloatExpr Float
    | NegationExpr (Node Expression)
    | StringExpr (List StringElement)
    | ParenthesizedExpr (Node Expression)
    | LetExpr (List (Node LetDeclaration)) (Node Expression)
    | AttributeSelectionExpr (Node Expression) (List (Node Name)) (Maybe (Node Expression))
    | FunctionExpr (Node Pattern) (Node Expression)
    | RecordExpr (List (Node Attribute))
    | ListExpr (List (Node Expression))
    | PathExpr Path
    | LookupPathExpr (List String)
    | WithExpr (Node Expression) (Node Expression)
    | AssertExpr (Node Expression) (Node Expression)
    | HasAttributeExpr (Node Expression) (Node AttrPath)


type alias Path =
    List (List StringElement)


type LetDeclaration
    = LetDeclaration (Node AttrPath) (Node Expression)
    | LetInheritVariables (List (Node String))
    | LetInheritFromSet (Node Expression) (List (Node String))


type Attribute
    = Attribute (Node AttrPath) (Node Expression)
    | AttributeInheritVariables (List (Node String))
    | AttributeInheritFromSet (Node Expression) (List (Node String))


type alias AttrPath =
    List (Node Name)


type StringElement
    = StringLiteral String
    | StringInterpolation (Node Expression)


type Name
    = StringName (List StringElement)
    | InterpolationName (Node Expression)
    | IdentifierName String


type Pattern
    = AllPattern
    | RecordPattern (List RecordFieldPattern) { open : Bool }
    | VarPattern String
    | AtPattern (Node Pattern) (Node String)
    | ReverseAtPattern (Node String) (Node Pattern)
    | ParenthesizedPattern (Node Pattern)


type RecordFieldPattern
    = RecordFieldPattern (Node String) (Maybe (Node Expression))

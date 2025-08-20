module Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), LetDeclaration(..), Name(..), Path, Pattern(..), RecordFieldPattern(..), StringElement(..))

{-|

@docs AttrPath, Attribute, Expression, LetDeclaration, Name, Path, Pattern, RecordFieldPattern, StringElement

-}

import Nix.Syntax.Node exposing (Node)


type Expression
    = NullExpr
    | ApplicationExpr (Node Expression) (List (Node Expression))
    | OperatorApplicationExpr (Node Expression) (Node String) (Node Expression)
    | VariableExpr String
      -- | IfBlock (Node Expression) (Node Expression) (Node Expression)
      -- | PrefixOperator String
    | BoolExpr Bool
    | IntExpr Int
      -- | Hex Int
    | FloatExpr Float
    | NegationExpr (Node Expression)
    | StringExpr (List StringElement)
      -- | CharLiteral Char
      -- | TupledExpression (List (Node Expression))
    | ParenthesizedExpr (Node Expression)
    | LetExpr (List (Node LetDeclaration)) (Node Expression)
      -- | CaseExpression CaseBlock
    | AttributeSelectionExpr (Node Expression) (List (Node String)) (Maybe (Node Expression))
      -- | RecordAccessFunction String
    | FunctionExpr (Node Pattern) (Node Expression)
    | RecordExpr (List (Node Attribute))
    | ListExpr (List (Node Expression))
    | PathExpr Path
    | WithExpr (Node Expression) (Node Expression)


type alias Path =
    List String


type LetDeclaration
    = LetDeclaration (Node String) (Node Expression)
    | LetInheritVariables (List (Node String))
    | LetInheritFromSet (Node String) (List (Node String))


type alias Attribute =
    ( Node AttrPath, Node Expression )


type alias AttrPath =
    List (Node Name)


type StringElement
    = StringLiteral String
    | StringInterpolation (Node Expression)


type Name
    = StringName (List StringElement)
    | IdentifierName String


type Pattern
    = AllPattern
      -- | UnitPattern
      -- | CharPattern Char
      -- | StringPattern String
      -- | IntPattern Int
      -- | HexPattern Int
      -- | FloatPattern Float
    | RecordPattern (List RecordFieldPattern) { open : Bool }
      -- | UnConsPattern (Node Pattern) (Node Pattern)
      -- | ListPattern (List (Node Pattern))
    | VarPattern String
      -- | AsPattern (Node Pattern) (Node String)
    | ParenthesizedPattern (Node Pattern)


type RecordFieldPattern
    = RecordFieldPattern (Node String) (Maybe (Node Expression))

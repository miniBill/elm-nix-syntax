module Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), LetDeclaration, Name(..), Pattern(..), RecordFieldPattern(..), StringElement(..))

import Nix.Syntax.Node exposing (Node)


type
    Expression
    -- = UnitExpr
    = ApplicationExpr (Node Expression) (List (Node Expression))
      -- | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
    | VariableExpr String
      -- | IfBlock (Node Expression) (Node Expression) (Node Expression)
      -- | PrefixOperator String
      -- | Operator String
      -- | Integer Int
      -- | Hex Int
      -- | Floatable Float
      -- | Negation (Node Expression)
    | StringExpr (List StringElement)
      -- | CharLiteral Char
      -- | TupledExpression (List (Node Expression))
    | ParenthesizedExpression (Node Expression)
    | LetExpression (List (Node LetDeclaration)) (Node Expression)
      -- | CaseExpression CaseBlock
    | AttributeSelection (Node Expression) (List (Node String)) (Maybe (Node Expression))
      -- | RecordAccessFunction String
      -- | RecordUpdateExpression (Node String) (List (Node RecordSetter))
    | FunctionExpr (Node Pattern) (Node Expression)
    | RecordExpr (List (Node Attribute))
    | ListExpr (List (Node Expression))


type alias LetDeclaration =
    ( Node String, Node Expression )


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

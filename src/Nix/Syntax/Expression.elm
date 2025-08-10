module Nix.Syntax.Expression exposing (..)

import Nix.Syntax.Node exposing (Node)


type
    Expression
    -- = UnitExpr
    -- | Application (List (Node Expression))
    -- | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
    -- | FunctionOrValue ModuleName String
    -- | IfBlock (Node Expression) (Node Expression) (Node Expression)
    -- | PrefixOperator String
    -- | Operator String
    -- | Integer Int
    -- | Hex Int
    -- | Floatable Float
    -- | Negation (Node Expression)
    = StringExpr (List StringElement)
      -- | CharLiteral Char
      -- | TupledExpression (List (Node Expression))
      -- | ParenthesizedExpression (Node Expression)
      -- | LetExpression LetBlock
      -- | CaseExpression CaseBlock
      -- | LambdaExpression Lambda
    | RecordExpr (List (Node Attribute))
    | ListExpr (List (Node Expression))



-- | RecordAccess (Node Expression) (Node String)
-- | RecordAccessFunction String
-- | RecordUpdateExpression (Node String) (List (Node RecordSetter))


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

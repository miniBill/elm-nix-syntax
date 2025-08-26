module Nix.Parser.Extra exposing (errorToString)

import Json.Encode
import Nix.Parser
import Nix.Parser.Context exposing (Context(..))
import Nix.Parser.Problem exposing (Problem(..))
import Parser.Error


errorToString :
    { cyan : String -> String
    , red : String -> String
    }
    -> String
    -> List Nix.Parser.DeadEnd
    -> String
errorToString { cyan, red } src deadEnds =
    Parser.Error.renderError
        { text = identity
        , formatContext = cyan
        , formatCaret = red
        , newline = "\n"
        , linesOfExtraContext = 3
        }
        { contextStack =
            \{ contextStack } ->
                List.map
                    (\{ row, col, context } ->
                        { row = row
                        , col = col
                        , context = contextToString context
                        }
                    )
                    contextStack
        , problemToString = problemToExpected
        }
        src
        deadEnds
        |> String.concat


contextToString : Context -> String
contextToString context =
    case context of
        ParsingExpression ->
            "expression"

        ParsingString ->
            "string"

        ParsingList ->
            "list"

        ParsingAttrset ->
            "attrset"

        ParsingFunction ->
            "function"

        ParsingLet ->
            "let/in"

        ParsingIfThenElse ->
            "if/then/else"

        ParsingApplication ->
            "application"

        ParsingPattern ->
            "pattern"


problemToExpected : Problem -> Parser.Error.Expected
problemToExpected problem =
    case problem of
        Expecting t ->
            Parser.Error.Expected (escape t)

        ExpectingEnd ->
            Parser.Error.Expected "the end"

        ExpectingVariable ->
            Parser.Error.Expected "a variable"

        ExpectingIdentifier ->
            Parser.Error.Expected "an identifier"

        ExpectingDigit ->
            Parser.Error.Expected "a digit"

        Unexpected t ->
            Parser.Error.Other t

        Unimplemented t ->
            Parser.Error.Other ("Unimplemented: " ++ t)


escape : String -> String
escape s =
    Json.Encode.encode 0 (Json.Encode.string s)

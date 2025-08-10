module Nix.Parser exposing (errorToString, parse)

import Ansi.Color
import List.Extra
import Nix.Syntax.Expression
    exposing
        ( AttrPath
        , Attribute
        , Expression(..)
        , Name(..)
        , Pattern(..)
        , StringElement(..)
        )
import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location)
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Parser.Advanced.Workaround
import Set exposing (Set)


type Context
    = ParsingExpression
    | ParsingString
    | ParsingList
    | ParsingAttrset
    | ParsingFunction


type Problem
    = ExpectingEnd
    | Expecting String
    | ExpectingVariable
    | UnexpectedChar
    | Unimplemented String


type alias Parser a =
    Parser.Parser Context Problem a


type alias DeadEnd =
    Parser.DeadEnd Context Problem


parse : String -> Result (List DeadEnd) (Node Expression)
parse input =
    Parser.run
        (Parser.succeed identity
            |. spaces
            |= expression
            |. Parser.end ExpectingEnd
        )
        input


expression : Parser (Node Expression)
expression =
    Parser.inContext ParsingExpression <|
        nodify
            (Parser.oneOf
                [ Parser.map StringExpr string
                , Parser.map RecordExpr attributeSet
                , Parser.map ListExpr list
                , function
                ]
            )
            |. spaces


function : Parser Expression
function =
    Parser.succeed FunctionExpr
        |= Parser.backtrackable pattern
        |. Parser.backtrackable spaces
        |. Parser.symbol (token ":")
        |= Parser.inContext ParsingFunction
            (Parser.succeed identity
                |. spaces
                |= Parser.lazy (\_ -> expression)
            )


pattern : Parser (Node Pattern)
pattern =
    nodify
        (Parser.oneOf
            [ Parser.succeed VarPattern
                |= identifier
            , Parser.succeed AllPattern
                |. Parser.symbol (token "_")
            , Parser.succeed identity
                |. Parser.symbol (token "{")
                |= Parser.oneOf
                    [ Parser.problem (Unimplemented "set pattern")

                    -- Rember defaults in params
                    , Parser.problem (Unimplemented "open set pattern")
                    ]
            , Parser.problem (Unimplemented "@-pattern")
            ]
        )


string : Parser (List StringElement)
string =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.symbol (token "\"")
            |= Parser.inContext ParsingString
                (Parser.succeed identity
                    |= many stringElement
                    |. Parser.symbol (token "\"")
                )
        , Parser.succeed identity
            |= Parser.problem (Unimplemented "indented string")
        , Parser.succeed identity
            |= Parser.problem (Unimplemented "uri")
        ]


token : String -> Token Problem
token t =
    Parser.Token t (Expecting t)


nodify : Parser a -> Parser (Node a)
nodify inner =
    Parser.succeed
        (\start value end ->
            Node
                { start = start
                , end = end
                }
                value
        )
        |= location
        |= inner
        |= location


location : Parser Location
location =
    Parser.succeed
        (\row column ->
            { row = row
            , column = column
            }
        )
        |= Parser.getRow
        |= Parser.getCol


stringElement : Parser StringElement
stringElement =
    Parser.oneOf
        [ Parser.succeed (StringLiteral "$${")
            |. Parser.symbol (token "$${")
        , Parser.succeed StringInterpolation
            |. Parser.symbol (token "${")
            |= Parser.lazy (\_ -> expression)
            |. Parser.symbol (token "}")
        , Parser.succeed (\chars -> StringLiteral (String.fromList chars))
            |= some stringChar
        ]


some : Parser a -> Parser (List a)
some inner =
    Parser.succeed (::)
        |= inner
        |= many inner


stringChar : Parser Char
stringChar =
    Parser.oneOf
        [ Parser.succeed '\\'
            |. Parser.symbol (token "\\\\")
        , Parser.succeed '"'
            |. Parser.symbol (token "\\\"")
        , Parser.succeed '$'
            |. Parser.symbol (token "\\$")
        , Parser.succeed '\u{000D}'
            |. Parser.symbol (token "\\r")
        , Parser.succeed '\n'
            |. Parser.symbol (token "\\n")
        , Parser.succeed '\t'
            |. Parser.symbol (token "\\t")
        , Parser.chompIf (\c -> c /= '\\' && c /= '"')
            (Expecting "String character")
            |> Parser.getChompedString
            |> Parser.andThen
                (\c ->
                    case String.toList c of
                        [ x ] ->
                            Parser.succeed x

                        _ ->
                            Parser.problem UnexpectedChar
                )
        ]


many : Parser a -> Parser (List a)
many item =
    Parser.sequence
        { start = token ""
        , end = token ""
        , spaces = Parser.succeed ()
        , item = item
        , trailing = Parser.Optional
        , separator = token ""
        }


attributeSet : Parser (List (Node Attribute))
attributeSet =
    Parser.succeed identity
        |. Parser.symbol (token "{")
        |= Parser.inContext ParsingAttrset
            (Parser.sequence
                { start = token ""
                , spaces = spaces
                , end = token "}"
                , trailing = Parser.Optional
                , separator = token ""
                , item = attribute
                }
            )


attribute : Parser (Node Attribute)
attribute =
    nodify
        (Parser.succeed Tuple.pair
            |= attrPath
            |. spaces
            |. Parser.symbol (token "=")
            |. spaces
            |= Parser.lazy (\_ -> expression)
            |. spaces
            |. Parser.symbol (token ";")
        )


attrPath : Parser (Node AttrPath)
attrPath =
    nodify
        (Parser.sequence
            { start = token ""
            , end = token ""
            , separator = token "."
            , spaces = Parser.succeed ()
            , trailing = Parser.Forbidden
            , item = name
            }
        )


name : Parser (Node Name)
name =
    nodify
        (Parser.oneOf
            [ Parser.succeed StringName
                |= string
            , Parser.succeed IdentifierName
                |= identifier
            ]
        )


identifier : Parser String
identifier =
    let
        start : Char -> Bool
        start c =
            c == '_' || Char.isAlpha c

        inner : Char -> Bool
        inner c =
            c == '_' || c == '\'' || c == '-' || Char.isAlphaNum c
    in
    Parser.variable
        { start = start
        , inner = inner
        , reserved = reserved
        , expecting = ExpectingVariable
        }


reserved : Set String
reserved =
    Set.fromList
        [ "let"
        , "in"
        ]


list : Parser (List (Node Expression))
list =
    Parser.succeed identity
        |. Parser.symbol (token "[")
        |= Parser.inContext ParsingList
            (Parser.sequence
                { start = token ""
                , separator = token ""
                , end = token "]"
                , spaces = spaces
                , trailing = Parser.Optional
                , item = Parser.lazy (\_ -> expression)
                }
            )


spaces : Parser ()
spaces =
    innerSpaces
        |. Parser.oneOf
            [ comment |. Parser.lazy (\_ -> spaces)
            , Parser.succeed ()
            ]


comment : Parser ()
comment =
    Parser.Advanced.Workaround.lineCommentAfter (token "#")


innerSpaces : Parser ()
innerSpaces =
    Parser.chompWhile
        (\c -> c == ' ' || c == '\t' || c == '\n')


errorToString : String -> List DeadEnd -> String
errorToString src deadEnds =
    let
        lines =
            src |> String.split "\n"
    in
    deadEnds
        |> List.Extra.gatherEqualsBy
            (\{ row, col } -> ( row, col ))
        |> List.map (deadEndToString lines)
        |> String.join "\n"


deadEndToString : List String -> ( DeadEnd, List DeadEnd ) -> String
deadEndToString lines ( head, tail ) =
    let
        line : String
        line =
            lines
                |> List.drop (head.row - 1)
                |> List.head
                |> Maybe.withDefault ""

        grouped :
            List
                ( List { row : Int, col : Int, context : Context }
                , List Problem
                )
        grouped =
            (head :: tail)
                |> List.Extra.gatherEqualsBy .contextStack
                |> List.map
                    (\( ihead, itail ) ->
                        ( ihead.contextStack
                        , List.map .problem (ihead :: itail)
                        )
                    )

        sourceFragment : String
        sourceFragment =
            let
                num =
                    String.fromInt head.row
            in
            "\n"
                ++ num
                ++ "|"
                ++ line
                ++ "\n"
                ++ String.repeat (String.length num + head.col) " "
                ++ Ansi.Color.fontColor Ansi.Color.red "^"
    in
    case grouped of
        multiple ->
            String.join "\n"
                (sourceFragment
                    :: List.map
                        (\( contextStack, problems ) ->
                            let
                                expected =
                                    problems
                                        |> List.filterMap
                                            (\problem ->
                                                case problem of
                                                    Expecting x ->
                                                        Just x

                                                    _ ->
                                                        Nothing
                                            )

                                other =
                                    problems
                                        |> List.filterMap
                                            (\problem ->
                                                case problem of
                                                    Expecting _ ->
                                                        Nothing

                                                    _ ->
                                                        Just (problemToString problem)
                                            )

                                groupedExpected : List String
                                groupedExpected =
                                    case expected of
                                        [] ->
                                            []

                                        [ x ] ->
                                            [ "expecting '" ++ x ++ "'" ]

                                        _ ->
                                            [ "expecting one of '"
                                                ++ String.join "' '" expected
                                                ++ "'"
                                            ]

                                problemsString =
                                    (groupedExpected ++ other)
                                        |> List.sort
                                        |> String.join "\n  "
                            in
                            "- "
                                ++ Ansi.Color.fontColor
                                    Ansi.Color.cyan
                                    (contextStackToString contextStack)
                                ++ ":\n  "
                                ++ problemsString
                        )
                        multiple
                )


contextStackToString : List { row : Int, col : Int, context : Context } -> String
contextStackToString frames =
    frames
        |> List.reverse
        |> List.map
            (\{ row, col, context } ->
                contextToString context
                    ++ " ("
                    ++ String.fromInt row
                    ++ ":"
                    ++ String.fromInt col
                    ++ ")"
            )
        |> String.join " > "


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


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        ExpectingVariable ->
            "expecting variable name"

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Unimplemented s ->
            "unimplemented: " ++ s

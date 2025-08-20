module Nix.Parser exposing (errorToString, parse, pattern)

import Ansi.Color
import List.Extra
import Nix.Syntax.Expression
    exposing
        ( AttrPath
        , Attribute
        , Expression(..)
        , LetDeclaration
        , Name(..)
        , Path
        , Pattern(..)
        , RecordFieldPattern(..)
        , StringElement(..)
        )
import Nix.Syntax.Node as Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location)
import Parser.Advanced as Parser exposing ((|.), (|=), Step(..), Token(..), andThen, backtrackable, chompIf, chompWhile, end, getChompedString, inContext, keyword, lazy, map, oneOf, problem, succeed, symbol)
import Parser.Advanced.Workaround
import Set exposing (Set)


type Context
    = ParsingExpression
    | ParsingString
    | ParsingList
    | ParsingAttrset
    | ParsingFunction
    | ParsingLet
    | ParsingApplication
    | ParsingPattern


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
        (succeed identity
            |. spaces
            |= expression
            |. end ExpectingEnd
        )
        input


expression : Parser (Node Expression)
expression =
    inContext ParsingExpression
        (oneOf
            [ node letIn
            , node function
            , expression_14_logicalImplication
            ]
            |. spaces
        )


expression_14_logicalImplication : Parser (Node Expression)
expression_14_logicalImplication =
    oneOf
        [ expression_13_logicalDisjunction
        ]


expression_13_logicalDisjunction : Parser (Node Expression)
expression_13_logicalDisjunction =
    oneOf
        [ expression_12_logicalConjunction
        ]


expression_12_logicalConjunction : Parser (Node Expression)
expression_12_logicalConjunction =
    oneOf
        [ expression_11_equality
        ]


expression_11_equality : Parser (Node Expression)
expression_11_equality =
    oneOf
        [ expression_10_comparison
        ]


expression_10_comparison : Parser (Node Expression)
expression_10_comparison =
    oneOf
        [ expression_9_update ]


expression_9_update : Parser (Node Expression)
expression_9_update =
    node
        (succeed (\x f -> f x)
            |= expression_8_logicalNegation
            |. spaces
            |= oneOf
                [ succeed (\r l -> UpdateExpr l r)
                    |. symbol (token "//")
                    |. spaces
                    |= lazy (\_ -> expression_9_update)
                , succeed Node.value
                ]
        )


expression_8_logicalNegation : Parser (Node Expression)
expression_8_logicalNegation =
    oneOf
        [ expression_7_additionSubtraction
        ]


expression_7_additionSubtraction : Parser (Node Expression)
expression_7_additionSubtraction =
    oneOf
        [ expression_6_multiplicationDivision
        ]


expression_6_multiplicationDivision : Parser (Node Expression)
expression_6_multiplicationDivision =
    oneOf
        [ expression_5_concatenation
        ]


expression_5_concatenation : Parser (Node Expression)
expression_5_concatenation =
    oneOf
        [ expression_4_hasAttribute
        ]


expression_4_hasAttribute : Parser (Node Expression)
expression_4_hasAttribute =
    oneOf
        [ expression_3_negation
        ]


expression_3_negation : Parser (Node Expression)
expression_3_negation =
    oneOf
        [ node
            (succeed NegationExpr
                |. symbol (token "-")
                |. spaces
                |= expression_2_application
            )
        , expression_2_application
        ]


expression_2_application : Parser (Node Expression)
expression_2_application =
    oneOf
        [ node application
            |. spaces
        , expression_1_attributeSelection
        ]


application : Parser Expression
application =
    succeed (\head tail -> ApplicationExpr head tail)
        |= backtrackable expression_1_attributeSelection
        |= inContext ParsingApplication (some expression_1_attributeSelection)


expression_1_attributeSelection : Parser (Node Expression)
expression_1_attributeSelection =
    node
        (oneOf
            [ succeed Tuple.pair
                |= expression_0_atom
                |= many
                    (succeed identity
                        |. symbol (token ".")
                        |= node identifier
                    )
                |> andThen
                    (\( atom, identifiers ) ->
                        if List.isEmpty identifiers then
                            succeed (Node.value atom)

                        else
                            succeed (AttributeSelectionExpr atom identifiers)
                                |. spaces
                                |= oneOf
                                    [ succeed Just
                                        |. keyword (token "or")
                                        |. spaces
                                        |= lazy (\_ -> expression)
                                    , succeed Nothing
                                    ]
                    )
            ]
            |. spaces
        )


expression_0_atom : Parser (Node Expression)
expression_0_atom =
    node
        (oneOf
            [ map StringExpr string
            , map RecordExpr attributeSet
            , map ListExpr list
            , map ParenthesizedExpr parenthesizedExpression
            , map VariableExpr identifier
            , map PathExpr path
            ]
        )


path : Parser Path
path =
    let
        valid : Char -> Bool
        valid c =
            c /= ' ' && c /= '/' && c /= ';'
    in
    oneOf
        [ succeed (::)
            |= (succeed "."
                    |. symbol (token ".")
               )
            |= many
                (succeed identity
                    |. symbol (token "/")
                    |= getChompedString
                        (succeed ()
                            |. chompIf valid (Expecting "path piece")
                            |. chompWhile valid
                        )
                )
        ]


parenthesizedExpression : Parser (Node Expression)
parenthesizedExpression =
    succeed identity
        |. symbol (token "(")
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. symbol (token ")")


letIn : Parser Expression
letIn =
    succeed identity
        |. keyword (token "let")
        |. spaces
        |= inContext ParsingLet
            (succeed LetExpr
                |= many letDeclaration
                |. keyword (token "in")
                |. spaces
                |= lazy (\_ -> expression)
            )


letDeclaration : Parser (Node LetDeclaration)
letDeclaration =
    node
        (succeed Tuple.pair
            |= node identifier
            |. spaces
            |. symbol (token "=")
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol (token ";")
            |. spaces
        )


function : Parser Expression
function =
    succeed FunctionExpr
        |= backtrackable pattern
        |. symbol (token ":")
        |= inContext ParsingFunction
            (succeed identity
                |. spaces
                |= lazy (\_ -> expression)
            )


pattern : Parser (Node Pattern)
pattern =
    let
        inner : List (Parser Pattern)
        inner =
            [ Parser.succeed
                (\items ->
                    RecordPattern
                        (List.filterMap identity items)
                        { open = List.member Nothing items }
                )
                |= Parser.sequence
                    { start = token "{"
                    , item =
                        Parser.oneOf
                            [ succeed Just
                                |= recordFieldPattern
                            , succeed Nothing
                                |. symbol (token "...")
                            ]
                    , end = token "}"
                    , separator = token ","
                    , spaces = spaces
                    , trailing = Parser.Optional
                    }
            , succeed VarPattern
                |= identifier
            , succeed AllPattern
                |. symbol (token "_")
            , problem (Unimplemented "@-pattern")
            ]
    in
    node
        (inContext ParsingPattern
            (oneOf inner)
        )
        |. spaces


recordFieldPattern : Parser RecordFieldPattern
recordFieldPattern =
    succeed RecordFieldPattern
        |= node identifier
        |. spaces
        |= oneOf
            [ succeed Just
                |. symbol (token "?")
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
            , succeed Nothing
            ]


string : Parser (List StringElement)
string =
    oneOf
        [ succeed identity
            |. symbol (token "\"")
            |= inContext ParsingString
                (succeed identity
                    |= many stringElement
                    |. symbol (token "\"")
                )
        , succeed identity
            |= problem (Unimplemented "indented string")
        , succeed identity
            |= problem (Unimplemented "uri")
        ]


token : String -> Token Problem
token t =
    Parser.Token t (Expecting t)


node : Parser a -> Parser (Node a)
node inner =
    succeed
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
    succeed
        (\row column ->
            { row = row
            , column = column
            }
        )
        |= Parser.getRow
        |= Parser.getCol


stringElement : Parser StringElement
stringElement =
    oneOf
        [ succeed (StringLiteral "$${")
            |. symbol (token "$${")
        , succeed StringInterpolation
            |. symbol (token "${")
            |= lazy (\_ -> expression)
            |. symbol (token "}")
        , succeed (\chars -> StringLiteral (String.fromList chars))
            |= some stringChar
        ]


some : Parser a -> Parser (List a)
some inner =
    succeed (::)
        |= inner
        |= many inner


stringChar : Parser Char
stringChar =
    oneOf
        [ succeed '\\'
            |. symbol (token "\\\\")
        , succeed '"'
            |. symbol (token "\\\"")
        , succeed '$'
            |. symbol (token "\\$")
        , succeed '\u{000D}'
            |. symbol (token "\\r")
        , succeed '\n'
            |. symbol (token "\\n")
        , succeed '\t'
            |. symbol (token "\\t")
        , Parser.chompIf (\c -> c /= '\\' && c /= '"')
            (Expecting "String character")
            |> Parser.getChompedString
            |> Parser.andThen
                (\c ->
                    case String.toList c of
                        [ x ] ->
                            succeed x

                        _ ->
                            problem UnexpectedChar
                )
        ]


many : Parser a -> Parser (List a)
many item =
    Parser.sequence
        { start = token ""
        , end = token ""
        , spaces = succeed ()
        , item = item
        , trailing = Parser.Optional
        , separator = token ""
        }


attributeSet : Parser (List (Node Attribute))
attributeSet =
    succeed identity
        |. symbol (token "{")
        |= inContext ParsingAttrset
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
    node
        (succeed Tuple.pair
            |= attrPath
            |. spaces
            |. symbol (token "=")
            |. spaces
            |= lazy (\_ -> expression)
            |. spaces
            |. symbol (token ";")
        )


attrPath : Parser (Node AttrPath)
attrPath =
    node
        (Parser.sequence
            { start = token ""
            , end = token ""
            , separator = token "."
            , spaces = succeed ()
            , trailing = Parser.Forbidden
            , item = name
            }
        )


name : Parser (Node Name)
name =
    node
        (oneOf
            [ succeed StringName
                |= string
            , succeed IdentifierName
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
        , "with"
        , "inherit"
        ]


list : Parser (List (Node Expression))
list =
    succeed identity
        |. symbol (token "[")
        |= inContext ParsingList
            (Parser.sequence
                { start = token ""
                , separator = token ""
                , end = token "]"
                , spaces = spaces
                , trailing = Parser.Optional
                , item = lazy (\_ -> expression_1_attributeSelection)
                }
            )


spaces : Parser ()
spaces =
    innerSpaces
        |. oneOf
            [ comment |. lazy (\_ -> spaces)
            , succeed ()
            ]


comment : Parser ()
comment =
    Parser.oneOf
        [ Parser.Advanced.Workaround.lineCommentAfter (token "#")
        , Parser.Advanced.Workaround.multiCommentAfter (token "/*") (token "*/") Parser.NotNestable
        ]


innerSpaces : Parser ()
innerSpaces =
    Parser.chompWhile
        (\c -> c == ' ' || c == '\t' || c == '\n')


errorToString : String -> List DeadEnd -> String
errorToString src deadEnds =
    let
        lines =
            src
                |> String.split "\n"
                |> List.indexedMap (\i l -> ( i + 1, l ))
    in
    deadEnds
        |> List.Extra.gatherEqualsBy
            (\{ row, col } -> ( row, col ))
        |> List.map (deadEndToString lines)
        |> String.join "\n"


deadEndToString : List ( Int, String ) -> ( DeadEnd, List DeadEnd ) -> String
deadEndToString lines ( head, tail ) =
    let
        line : ( Int, String )
        line =
            lines
                |> List.drop (head.row - 1)
                |> List.head
                |> Maybe.withDefault ( head.row, "" )

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

        sourceFragment : List String
        sourceFragment =
            let
                before =
                    lines
                        |> List.drop (head.row - 3)
                        |> List.take 3
                        |> List.Extra.takeWhile (\( i, _ ) -> i < head.row)

                after =
                    lines
                        |> List.drop head.row
                        |> List.take 3

                formatLine : ( Int, String ) -> String
                formatLine ( row, l ) =
                    String.padLeft numLength ' ' (String.fromInt row)
                        ++ "| "
                        ++ l

                numLength : Int
                numLength =
                    after
                        |> List.Extra.last
                        |> Maybe.map (\( r, _ ) -> r)
                        |> Maybe.withDefault head.row
                        |> String.fromInt
                        |> String.length

                caret =
                    String.repeat (numLength + head.col + 1) " "
                        ++ Ansi.Color.fontColor Ansi.Color.red "^"
            in
            List.map formatLine before
                ++ formatLine line
                :: caret
                :: List.map formatLine after

        groupToString :
            ( List { row : Int, col : Int, context : Context }
            , List Problem
            )
            -> List String
        groupToString ( contextStack, problems ) =
            let
                expected : List String
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

                other : List String
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
            [ "- "
                ++ Ansi.Color.fontColor
                    Ansi.Color.cyan
                    (contextStackToString contextStack)
                ++ ":"
            , "  " ++ problemsString
            ]
    in
    (sourceFragment ++ "" :: List.concatMap groupToString grouped)
        |> String.join "\n"


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

        ParsingLet ->
            "let"

        ParsingApplication ->
            "application"

        ParsingPattern ->
            "pattern"


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

module Nix.Parser exposing (errorToString, parse, pattern)

import Ansi.Color
import List.Extra
import Maybe.Extra
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
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..), andThen, backtrackable, chompIf, chompWhile, end, getChompedString, inContext, keyword, lazy, map, oneOf, problem, sequence, succeed, symbol)
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
    | ExpectingDigit
    | Unexpected String
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
        (succeed Tuple.pair
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
        )
        |. spaces


expression_0_atom : Parser (Node Expression)
expression_0_atom =
    node
        (oneOf
            [ map StringExpr string
            , number
            , map RecordExpr attributeSet
            , map ListExpr list
            , map ParenthesizedExpr parenthesizedExpression
            , map VariableExpr identifier
            , map PathExpr path
            ]
        )


number : Parser Expression
number =
    succeed
        (\n b a ->
            case a of
                Nothing ->
                    if n then
                        IntExpr -b

                    else
                        IntExpr b

                Just ae ->
                    if n then
                        FloatExpr -(toFloat b + ae)

                    else
                        FloatExpr (toFloat b + ae)
        )
        |= oneOf
            [ succeed True
                |. backtrackable (symbol (token "-"))
            , succeed False
            ]
        |= (succeed ()
                |. chompIf Char.isDigit ExpectingDigit
                |. chompWhile Char.isDigit
                |> getChompedString
                |> andThen
                    (\r ->
                        case String.toInt r of
                            Just i ->
                                succeed i

                            Nothing ->
                                -- This shouldn't happen
                                problem (Expecting "A valid integer")
                    )
           )
        |= oneOf
            [ succeed Just
                |. symbol (token ".")
                |= problem (Unimplemented "Float")
            , succeed Nothing
            ]


path : Parser Path
path =
    let
        valid : Char -> Bool
        valid c =
            c /= ' ' && c /= '/' && c /= ';'
    in
    oneOf
        [ succeed (::)
            |= oneOf
                [ succeed "."
                    |. symbol (token ".")
                , succeed ".."
                    |. symbol (token "..")
                ]
            |= many
                (succeed identity
                    |. symbol (token "/")
                    |= getChompedString
                        (succeed ()
                            |. chompIf valid (Expecting "path piece")
                            |. chompWhile valid
                        )
                )
        , problem (Unimplemented "<> paths")
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
            [ succeed
                (\items ->
                    RecordPattern
                        (List.filterMap identity items)
                        { open = List.member Nothing items }
                )
                |= sequence
                    { start = token "{"
                    , item =
                        oneOf
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
                    |= many (stringElement { indented = False })
                    |. symbol (token "\"")
                )
        , succeed identity
            |= indentedString
        , succeed identity
            |= problem (Unimplemented "uri")
        ]


indentedString : Parser (List StringElement)
indentedString =
    succeed cleanIndentation
        |. symbol (token "''")
        |= inContext ParsingString
            (sequence
                { start = token ""
                , end = token "''"
                , trailing = Parser.Optional
                , separator = Token "\n" (Expecting "\\n")
                , item = many (stringElement { indented = True })
                , spaces = succeed ()
                }
            )


cleanIndentation : List (List StringElement) -> List StringElement
cleanIndentation lines =
    let
        withoutFirst : List (List StringElement)
        withoutFirst =
            case lines of
                [] :: tail ->
                    tail

                _ ->
                    lines

        withoutLast : List (List StringElement)
        withoutLast =
            case List.reverse withoutFirst of
                [] :: init ->
                    List.reverse init

                [ StringLiteral s ] :: init ->
                    if String.isEmpty (String.trim s) then
                        List.reverse init

                    else
                        withoutFirst

                _ ->
                    withoutFirst

        commonIndentation : Int
        commonIndentation =
            withoutLast
                |> List.map findIndentation
                |> List.minimum
                |> Maybe.withDefault 0

        unindent : List StringElement -> Maybe (List StringElement)
        unindent line =
            case line of
                (StringLiteral s) :: tail ->
                    Just (StringLiteral (String.dropLeft commonIndentation s) :: tail)

                _ ->
                    -- Something has gone wrong
                    Nothing

        unindented : List (List StringElement)
        unindented =
            if commonIndentation > 0 then
                Maybe.Extra.combineMap unindent withoutLast
                    |> Maybe.withDefault withoutLast

            else
                withoutLast
    in
    unindented
        |> List.Extra.intercalate [ StringLiteral "\n" ]
        |> simplifyString


findIndentation : List StringElement -> Int
findIndentation elements =
    case elements of
        (StringLiteral s) :: _ ->
            s |> String.toList |> countWhileSpace

        _ ->
            0


countWhileSpace : List Char -> Int
countWhileSpace c =
    let
        go : Int -> List Char -> Int
        go acc q =
            case q of
                ' ' :: tail ->
                    go (acc + 1) tail

                _ ->
                    acc
    in
    go 0 c


simplifyString : List StringElement -> List StringElement
simplifyString elements =
    let
        ( last, acc ) =
            List.foldl
                (\e ( l, a ) ->
                    case e of
                        StringInterpolation _ ->
                            case l of
                                Nothing ->
                                    ( Nothing, e :: a )

                                Just s ->
                                    ( Nothing, e :: StringLiteral s :: a )

                        StringLiteral el ->
                            case l of
                                Nothing ->
                                    ( Just el, a )

                                Just s ->
                                    ( Just (s ++ el), a )
                )
                ( Nothing, [] )
                elements
    in
    case last of
        Nothing ->
            List.reverse acc

        Just l ->
            List.reverse (StringLiteral l :: acc)


token : String -> Token Problem
token t =
    Token t (Expecting t)


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


stringElement : { indented : Bool } -> Parser StringElement
stringElement indented =
    oneOf
        [ succeed (StringLiteral "$${")
            |. symbol (token "$${")
        , succeed StringInterpolation
            |. symbol (token "${")
            |. spaces
            |= lazy (\_ -> expression)
            |. symbol (token "}")
        , succeed (\chars -> StringLiteral (String.fromList (List.concat chars)))
            |= some (stringChar indented)
        ]


some : Parser a -> Parser (List a)
some inner =
    succeed (::)
        |= inner
        |= many inner


stringChar : { indented : Bool } -> Parser (List Char)
stringChar { indented } =
    oneOf
        [ succeed [ '\\' ]
            |. symbol (token "\\\\")
        , succeed [ '"' ]
            |. symbol (token "\\\"")
        , succeed [ '$' ]
            |. symbol (token "\\$")
        , succeed [ '$' ]
            |. backtrackable (symbol (token "$"))
            |. (succeed String.dropLeft
                    |= Parser.getOffset
                    |= Parser.getSource
                    |> andThen
                        (\cut ->
                            if String.startsWith "{" cut then
                                problem (Unexpected "{")

                            else
                                succeed ()
                        )
               )
        , succeed [ '\u{000D}' ]
            |. symbol (token "\\r")
        , succeed [ '\n' ]
            |. symbol (token "\\n")
        , succeed [ '\t' ]
            |. symbol (token "\\t")
        , if indented then
            let
                notEnding : Parser.Parser c Problem ()
                notEnding =
                    succeed String.dropLeft
                        |= Parser.getOffset
                        |= Parser.getSource
                        |> andThen
                            (\cut ->
                                if String.startsWith "''" cut then
                                    problem (Expecting "char")

                                else
                                    succeed ()
                            )
            in
            succeed String.toList
                |. backtrackable notEnding
                |= (chompIf (\c -> c /= '\\' && c /= '"' && c /= '\n' && c /= '$')
                        (Expecting "String character")
                        |> getChompedString
                   )

          else
            succeed String.toList
                |= (chompIf (\c -> c /= '\\' && c /= '"' && c /= '\n' && c /= '$')
                        (Expecting "String character")
                        |> getChompedString
                   )
        ]


many : Parser a -> Parser (List a)
many item =
    sequence
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
            (sequence
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
        (sequence
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
            (sequence
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
    oneOf
        [ Parser.Advanced.Workaround.lineCommentAfter (token "#")
        , Parser.Advanced.Workaround.multiCommentAfter (token "/*") (token "*/") Parser.NotNestable
        ]


innerSpaces : Parser ()
innerSpaces =
    chompWhile
        (\c -> c == ' ' || c == '\t' || c == '\n')


errorToString : String -> List DeadEnd -> String
errorToString src deadEnds =
    let
        lines : List ( Int, String )
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
            formatSourceFragment head lines

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

                                    ExpectingEnd ->
                                        Nothing

                                    ExpectingVariable ->
                                        Nothing

                                    ExpectingDigit ->
                                        Nothing

                                    Unimplemented _ ->
                                        Nothing

                                    Unexpected _ ->
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

                                    ExpectingEnd ->
                                        Just (problemToString problem)

                                    ExpectingVariable ->
                                        Just (problemToString problem)

                                    ExpectingDigit ->
                                        Just (problemToString problem)

                                    Unimplemented _ ->
                                        Just (problemToString problem)

                                    Unexpected _ ->
                                        Just (problemToString problem)
                            )

                groupedExpected : List String
                groupedExpected =
                    case expected of
                        [] ->
                            []

                        [ x ] ->
                            [ "expecting '" ++ x ++ "'" ]

                        _ :: _ :: _ ->
                            [ "expecting one of '"
                                ++ String.join "' '" expected
                                ++ "'"
                            ]

                problemsString : String
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


formatSourceFragment : DeadEnd -> List ( Int, String ) -> List String
formatSourceFragment head lines =
    let
        line : ( Int, String )
        line =
            lines
                |> List.drop (head.row - 1)
                |> List.head
                |> Maybe.withDefault ( head.row, "" )

        before : List ( Int, String )
        before =
            lines
                |> List.drop (head.row - 3)
                |> List.take 3
                |> List.Extra.takeWhile (\( i, _ ) -> i < head.row)

        after : List ( Int, String )
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

        caret : String
        caret =
            String.repeat (numLength + head.col + 1) " "
                ++ Ansi.Color.fontColor Ansi.Color.red "^"
    in
    List.map formatLine before
        ++ formatLine line
        :: caret
        :: List.map formatLine after


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

        ExpectingDigit ->
            "expecting a digit 0-9"

        ExpectingVariable ->
            "expecting variable name"

        ExpectingEnd ->
            "expecting end"

        Unimplemented s ->
            "unimplemented: " ++ s

        Unexpected s ->
            "unexpected " ++ s

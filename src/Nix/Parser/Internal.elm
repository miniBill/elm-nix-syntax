module Nix.Parser.Internal exposing (DeadEnd, Parser, expression, path, pattern, spaces)

import List.Extra
import Maybe.Extra
import Nix.Parser.Context exposing (Context(..))
import Nix.Parser.Problem exposing (Problem(..))
import Nix.Syntax.Expression
    exposing
        ( AttrPath
        , Attribute(..)
        , Expression(..)
        , LetDeclaration(..)
        , Name(..)
        , Path
        , Pattern(..)
        , RecordFieldPattern(..)
        , StringElement(..)
        )
import Nix.Syntax.Node as Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location, Range)
import Parser.Advanced as Parser exposing ((|.), (|=), Step(..), Token(..), andThen, backtrackable, chompIf, chompWhile, getChompedString, inContext, lazy, loop, map, oneOf, problem, sequence, succeed)
import Parser.Advanced.Workaround
import Set exposing (Set)


type alias Parser a =
    Parser.Parser Context Problem a


type alias DeadEnd =
    Parser.DeadEnd Context Problem


expression : Parser (Node Expression)
expression =
    inContext ParsingExpression
        (oneOf
            [ node letIn
            , node ifThenElse
            , node function
            , node with
            , node assert
            , expression_14_logicalImplication
            ]
            |. spaces
        )


with : Parser Expression
with =
    succeed WithExpr
        |. backtrackable (keyword "with")
        |. negativeLookahead "-"
        |. spaces
        |= lazy (\_ -> expression)
        |. symbol ";"
        |. spaces
        |= lazy (\_ -> expression)


assert : Parser Expression
assert =
    succeed AssertExpr
        |. keyword "assert"
        |. spaces
        |= lazy (\_ -> expression)
        |. symbol ";"
        |. spaces
        |= lazy (\_ -> expression)


expression_14_logicalImplication : Parser (Node Expression)
expression_14_logicalImplication =
    rightAssociativeOperators [ "->" ] expression_13_logicalDisjunction


expression_13_logicalDisjunction : Parser (Node Expression)
expression_13_logicalDisjunction =
    leftAssociativeOperators [ "||" ] expression_12_logicalConjunction


expression_12_logicalConjunction : Parser (Node Expression)
expression_12_logicalConjunction =
    leftAssociativeOperators [ "&&" ] expression_11_equality


expression_11_equality : Parser (Node Expression)
expression_11_equality =
    node
        (succeed (\x f -> f x)
            |= expression_10_comparison
            |. spaces
            |= oneOf
                [ succeed (\o r l -> OperatorApplicationExpr l o r)
                    |= oneOf
                        [ node (succeed "==" |. symbol "==")
                        , node (succeed "!=" |. symbol "!=")
                        ]
                    |. spaces
                    |= expression_10_comparison
                , succeed Node.value
                ]
        )


expression_10_comparison : Parser (Node Expression)
expression_10_comparison =
    node
        (succeed (\x f -> f x)
            |= expression_9_update
            |. spaces
            |= oneOf
                [ succeed (\o r l -> OperatorApplicationExpr l o r)
                    |= oneOf
                        [ node (succeed "<=" |. symbol "<=")
                        , node (succeed "<" |. symbol "<")
                        , node (succeed ">=" |. symbol ">=")
                        , node (succeed ">" |. symbol ">")
                        ]
                    |. spaces
                    |= expression_9_update
                , succeed Node.value
                ]
        )


expression_9_update : Parser (Node Expression)
expression_9_update =
    rightAssociativeOperators [ "//" ] expression_8_logicalNegation


expression_8_logicalNegation : Parser (Node Expression)
expression_8_logicalNegation =
    oneOf
        [ node
            (succeed NegationExpr
                |. symbol "!"
                |. spaces
                |= expression_7_additionSubtraction
            )
        , expression_7_additionSubtraction
        ]


expression_7_additionSubtraction : Parser (Node Expression)
expression_7_additionSubtraction =
    leftAssociativeOperators [ "+", "-" ] expression_6_multiplicationDivision


expression_6_multiplicationDivision : Parser (Node Expression)
expression_6_multiplicationDivision =
    leftAssociativeOperators [ "*", "/" ] expression_5_concatenation


expression_5_concatenation : Parser (Node Expression)
expression_5_concatenation =
    rightAssociativeOperators [ "++" ] expression_4_hasAttribute


expression_4_hasAttribute : Parser (Node Expression)
expression_4_hasAttribute =
    node
        (succeed (\x f -> f x)
            |= expression_3_negation
            |. spaces
            |= oneOf
                [ succeed (\r l -> HasAttributeExpr l r)
                    |. (succeed "?" |. symbol "?")
                    |. spaces
                    |= attrPath
                , succeed Node.value
                ]
        )


expression_3_negation : Parser (Node Expression)
expression_3_negation =
    oneOf
        [ node
            (succeed NegationExpr
                |. symbol "-"
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
            |. spaces
            |= many
                (succeed identity
                    |. backtrackable (symbol ".")
                    |= name
                )
            |> andThen
                (\( atom, attrs ) ->
                    if List.isEmpty attrs then
                        succeed (Node.value atom)

                    else
                        succeed (AttributeSelectionExpr atom attrs)
                            |. spaces
                            |= oneOf
                                [ succeed Just
                                    |. keyword "or"
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
            [ succeed NullExpr |. keyword "null"
            , map BoolExpr bool
            , map StringExpr string
            , number
            , map RecordExpr attributeSet
            , map ListExpr list
            , map ParenthesizedExpr parenthesizedExpression
            , map VariableExpr identifier
            , map PathExpr path
            , map LookupPathExpr lookupPath
            ]
        )


lookupPath : Parser (List String)
lookupPath =
    succeed identity
        |. backtrackable (symbol "<")
        |= (chompWhile
                (\c ->
                    let
                        code : Int
                        code =
                            Char.toCode c
                    in
                    (0x61 <= code && code <= 0x7A)
                        || (0x41 <= code && code <= 0x5A)
                        || (0x30 <= code && code <= 0x39)
                        || (code == {- '-' -} 0x2D)
                        || (code == {- '/' -} 0x2F)
                        || (code == {- '.' -} 0x2E)
                )
                |> getChompedString
                |> map (String.split "/")
           )
        |. symbol ">"


leftAssociativeOperators : List String -> Parser (Node Expression) -> Parser (Node Expression)
leftAssociativeOperators ops parseItem =
    let
        opParser : Parser (Node String)
        opParser =
            oneOf
                (List.map
                    (\op ->
                        node
                            (succeed op
                                |. backtrackable (symbol op)
                                |. negativeLookahead "/"
                            )
                    )
                    ops
                )

        step : Node Expression -> Parser (Step (Node Expression) (Node Expression))
        step acc =
            oneOf
                [ succeed
                    (\op item ->
                        let
                            range : Range
                            range =
                                { start = (Node.range acc).start
                                , end = (Node.range item).end
                                }
                        in
                        Loop (Node range (OperatorApplicationExpr acc op item))
                    )
                    |. backtrackable spaces
                    |= opParser
                    |. spaces
                    |= parseItem
                , succeed (Done acc)
                ]
    in
    parseItem |> andThen (\item -> loop item step)


negativeLookahead : String -> Parser ()
negativeLookahead forbidden =
    succeed (\offset source -> String.slice offset (offset + String.length forbidden) source)
        |= Parser.getOffset
        |= Parser.getSource
        |> andThen
            (\sliced ->
                if sliced == forbidden then
                    problem (Unexpected forbidden)

                else
                    succeed ()
            )


rightAssociativeOperators : List String -> Parser (Node Expression) -> Parser (Node Expression)
rightAssociativeOperators ops parseItem =
    let
        opParser : Parser (Node String)
        opParser =
            oneOf
                (List.map
                    (\op ->
                        node
                            (succeed op
                                |. symbol op
                            )
                    )
                    ops
                )
    in
    node
        (succeed (\x f -> f x)
            |= parseItem
            |. spaces
            |= oneOf
                [ succeed (\o r l -> OperatorApplicationExpr l o r)
                    |= opParser
                    |. spaces
                    |= lazy (\_ -> rightAssociativeOperators ops parseItem)
                , succeed Node.value
                ]
        )


bool : Parser Bool
bool =
    oneOf
        [ succeed True
            |. keyword "true"
        , succeed False
            |. keyword "false"
        ]


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
                |. backtrackable (symbol "-")
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
                |. symbol "."
                |. chompWhile Char.isDigit
                |> getChompedString
                |> andThen
                    (\r ->
                        case String.toFloat ("0" ++ r) of
                            Just f ->
                                succeed (Just f)

                            Nothing ->
                                -- This shouldn't happen
                                problem (Expecting "A float")
                    )
            , succeed Nothing
            ]


path : Parser Path
path =
    oneOf
        [ succeed (::)
            |= oneOf
                [ succeed [ StringLiteral ".." ]
                    |. symbol ".."
                , succeed [ StringLiteral "." ]
                    |. symbol "."
                , succeed [ StringLiteral "~" ]
                    |. symbol "~"
                ]
            |. symbol "/"
            |= sequence
                { start = token ""
                , end = token ""
                , separator = token "/"
                , spaces = succeed ()
                , item = some (stringElement InPath)
                , trailing = Parser.Forbidden
                }
        , succeed ((::) [ StringLiteral "" ])
            |. negativeLookahead "//"
            |= sequence
                { start = token "/"
                , end = token ""
                , separator = token "/"
                , spaces = succeed ()
                , item = some (stringElement InPath)
                , trailing = Parser.Forbidden
                }
        ]


parenthesizedExpression : Parser (Node Expression)
parenthesizedExpression =
    succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\_ -> expression)
        |. spaces
        |. symbol ")"


letIn : Parser Expression
letIn =
    succeed identity
        |. keyword "let"
        |. spaces
        |= inContext ParsingLet
            (succeed LetExpr
                |= many letDeclaration
                |. keyword "in"
                |. spaces
                |= lazy (\_ -> expression)
            )


ifThenElse : Parser Expression
ifThenElse =
    succeed identity
        |. keyword "if"
        |. spaces
        |= inContext ParsingIfThenElse
            (succeed IfThenElseExpr
                |= lazy (\_ -> expression)
                |. keyword "then"
                |. spaces
                |= lazy (\_ -> expression)
                |. keyword "else"
                |. spaces
                |= lazy (\_ -> expression)
            )


letDeclaration : Parser (Node LetDeclaration)
letDeclaration =
    node
        (oneOf
            [ succeed LetDeclaration
                |= attrPath
                |. spaces
                |. symbol "="
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ";"
            , succeed identity
                |. keyword "inherit"
                |. spaces
                |= oneOf
                    [ succeed LetInheritFromSet
                        |. symbol "("
                        |. spaces
                        |= lazy (\_ -> expression)
                        |. spaces
                        |. symbol ")"
                        |. spaces
                        |= identifiers
                    , succeed LetInheritVariables
                        |= identifiers
                    ]
                |. spaces
                |. symbol ";"
            ]
        )
        |. spaces


identifiers : Parser (List (Node String))
identifiers =
    sequence
        { item = node identifier
        , separator = token ""
        , spaces = spaces
        , trailing = Parser.Optional
        , start = token ""
        , end = token ""
        }


function : Parser Expression
function =
    succeed FunctionExpr
        |= backtrackable pattern
        |. symbol ":"
        |= inContext ParsingFunction
            (succeed identity
                |. spaces
                |= lazy (\_ -> expression)
            )


pattern : Parser (Node Pattern)
pattern =
    let
        atom : List (Parser Pattern)
        atom =
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
                                |. symbol "..."
                            ]
                    , end = token "}"
                    , separator = token ","
                    , spaces = spaces
                    , trailing = Parser.Optional
                    }
            , succeed VarPattern
                |= identifier
            , succeed AllPattern
                |. symbol "_"
            , succeed ParenthesizedPattern
                |. symbol "("
                |. spaces
                |= lazy (\_ -> pattern)
                |. symbol ")"
            ]

        inner : Parser (Node Pattern)
        inner =
            succeed identity
                |= node (oneOf atom)
                |. spaces
                |> andThen
                    (\l ->
                        case l of
                            Node lRange (VarPattern lvar) ->
                                oneOf
                                    [ succeed
                                        (\r ->
                                            Node
                                                { start = (Node.range l).start
                                                , end = (Node.range r).end
                                                }
                                                (ReverseAtPattern (Node lRange lvar) r)
                                        )
                                        |. symbol "@"
                                        |. spaces
                                        |= lazy (\_ -> pattern)
                                    , succeed l
                                    ]

                            _ ->
                                oneOf
                                    [ succeed
                                        (\r ->
                                            Node
                                                { start = (Node.range l).start
                                                , end = (Node.range r).end
                                                }
                                                (AtPattern l r)
                                        )
                                        |. symbol "@"
                                        |. spaces
                                        |= node identifier
                                    , succeed l
                                    ]
                    )
    in
    inContext ParsingPattern inner
        |. spaces


recordFieldPattern : Parser RecordFieldPattern
recordFieldPattern =
    succeed RecordFieldPattern
        |= node identifier
        |. spaces
        |= oneOf
            [ succeed Just
                |. symbol "?"
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
            , succeed Nothing
            ]


string : Parser (List StringElement)
string =
    oneOf
        [ succeed identity
            |. symbol "\""
            |= inContext ParsingString
                (succeed identity
                    |= many (stringElement InSinglelineString)
                    |. symbol "\""
                )
        , succeed identity
            |= indentedString
        , succeed identity
            |= problem (Unimplemented "uri")
        ]


indentedString : Parser (List StringElement)
indentedString =
    succeed cleanIndentation
        |. symbol "''"
        |= inContext ParsingString
            (sequence
                { start = token ""
                , end = token "''"
                , trailing = Parser.Optional
                , separator = Token "\n" (Expecting "\\n")
                , item = many (stringElement InMultilineString)
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


type StringElementKind
    = InSinglelineString
    | InMultilineString
    | InPath


stringElement : StringElementKind -> Parser StringElement
stringElement kind =
    oneOf
        [ succeed (StringLiteral "$${")
            |. symbol "$${"
        , succeed StringInterpolation
            |. symbol "${"
            |. spaces
            |= lazy (\_ -> expression)
            |. symbol "}"
        , succeed (\chars -> StringLiteral (String.fromList (List.concat chars)))
            |= some (stringChar kind)
        ]


some : Parser a -> Parser (List a)
some inner =
    succeed (::)
        |= inner
        |= many inner


stringChar : StringElementKind -> Parser (List Char)
stringChar kind =
    case kind of
        InMultilineString ->
            oneOf
                [ succeed [ '$' ]
                    |. symbol "''$"
                , succeed [ '\'', '\'' ]
                    |. symbol "'''"
                , succeed [ '\n' ]
                    |. symbol "''\\n"
                , succeed [ '\u{000D}' ]
                    |. symbol "''\\r"
                , succeed [ '\t' ]
                    |. symbol "''\\t"
                , succeed [ '}' ]
                    |. symbol "\\}"
                , succeed [ '$' ]
                    |. backtrackable (symbol "$")
                    |. negativeLookahead "{"
                , let
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
                    |= (chompIf
                            (\c ->
                                let
                                    code : Int
                                    code =
                                        Char.toCode c
                                in
                                code /= {- '\n' -} 0x0A && code /= {- '$' -} 0x24
                            )
                            (Expecting "String character")
                            |> getChompedString
                       )
                ]

        InSinglelineString ->
            oneOf
                [ succeed [ '\\' ]
                    |. symbol "\\\\"
                , succeed [ '"' ]
                    |. symbol "\\\""
                , succeed [ '$' ]
                    |. symbol "\\$"
                , succeed [ '\u{000D}' ]
                    |. symbol "\\r"
                , succeed [ '\n' ]
                    |. symbol "\\n"
                , succeed [ '\t' ]
                    |. symbol "\\t"
                , succeed [ '$' ]
                    |. backtrackable (symbol "$")
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
                , succeed String.toList
                    |= (chompIf
                            (\c ->
                                let
                                    code : Int
                                    code =
                                        Char.toCode c
                                in
                                (code /= {- '"' -} 0x22)
                                    -- && (code /= {- '\n' -} 0x0A)
                                    && (code /= {- '$' -} 0x24)
                            )
                            (Expecting "String character")
                            |> getChompedString
                       )
                ]

        InPath ->
            succeed String.toList
                |= (chompIf
                        acceptableInPath
                        (Expecting "String character")
                        |> getChompedString
                   )


acceptableInPath : Char -> Bool
acceptableInPath c =
    let
        code : Int
        code =
            Char.toCode c
    in
    (code /= {- '"' -} 0x22)
        && (code /= {- '\n' -} 0x0A)
        && (code /= {- '$' -} 0x24)
        && (code /= {- '/' -} 0x2F)
        && (code /= {- ';' -} 0x3B)
        && (code /= {- '(' -} 0x28)
        && (code /= {- ')' -} 0x29)
        && (code /= {- ' ' -} 0x20)
        && (code /= {- '}' -} 0x7D)
        && (code /= {- ',' -} 0x2C)


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
        |. symbol "{"
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
        (oneOf
            [ succeed Attribute
                |= attrPath
                |. spaces
                |. symbol "="
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol ";"
            , succeed identity
                |. keyword "inherit"
                |. spaces
                |= oneOf
                    [ succeed AttributeInheritFromSet
                        |. symbol "("
                        |. spaces
                        |= lazy (\_ -> expression)
                        |. spaces
                        |. symbol ")"
                        |. spaces
                        |= identifiers
                    , succeed AttributeInheritVariables
                        |= identifiers
                    ]
                |. spaces
                |. symbol ";"
            ]
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
            , succeed InterpolationName
                |. symbol "${"
                |. spaces
                |= lazy (\_ -> expression)
                |. spaces
                |. symbol "}"
            , succeed IdentifierName
                |= identifier
            ]
        )


identifier : Parser String
identifier =
    let
        start : Char -> Bool
        start c =
            let
                code : Int
                code =
                    Char.toCode c
            in
            (code == {- '_' -} 0x5F)
                || (0x61 {- 'a' -} <= code && code <= {- 'z' -} 0x7A)
                || (0x41 {- 'A' -} <= code && code <= {- 'Z' -} 0x5A)

        inner : Char -> Bool
        inner c =
            let
                code : Int
                code =
                    Char.toCode c
            in
            (code == {- '_' -} 0x5F)
                || (code == {- '\'' -} 0x27)
                || (code == {- '-' -} 0x2D)
                || (0x61 {- 'a' -} <= code && code <= {- 'z' -} 0x7A)
                || (0x41 {- 'A' -} <= code && code <= {- 'Z' -} 0x5A)
                || (0x30 {- '0' -} <= code && code <= {- '9' -} 0x39)
    in
    Parser.variable
        { start = start
        , inner = inner
        , reserved = reserved
        , expecting = ExpectingIdentifier
        }


reserved : Set String
reserved =
    Set.fromList
        [ "assert"
        , "else"
        , "if"
        , "in"
        , "inherit"
        , "let"
        , "then"
        , "with"
        ]


list : Parser (List (Node Expression))
list =
    succeed identity
        |. symbol "["
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
        (\c ->
            let
                code : Int
                code =
                    Char.toCode c
            in
            (code == 32 {- ' ' -})
                || (code == 9 {- \t -})
                || (code == 0x0A {- \n -})
        )


keyword : String -> Parser ()
keyword v =
    Parser.keyword (token v)


symbol : String -> Parser ()
symbol v =
    Parser.symbol (token v)

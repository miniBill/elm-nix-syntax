module Nix.Parser exposing (errorToString, parse)

import Ansi.Color
import List.Extra
import Nix.Syntax.Expression exposing (AttrPath, Attribute, Expression(..), Name(..), StringElement(..))
import Nix.Syntax.Node exposing (Node(..))
import Nix.Syntax.Range exposing (Location)
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..))
import Parser.Advanced.Workaround


type Context
    = ParsingExpression
    | ParsingString
    | ParsingList
    | ParsingAttrset


type Problem
    = ExpectingEnd
    | Problem String
    | Expecting String
    | UnexpectedChar


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
                ]
            )
            |. spaces


string : Parser (List StringElement)
string =
    Parser.inContext ParsingString <|
        Parser.oneOf
            [ Parser.succeed identity
                |. Parser.symbol (token "\"")
                |= many stringElement
                |. Parser.symbol (token "\"")
            , Parser.succeed identity
                |= Parser.problem (Problem "indented string")
            , Parser.succeed identity
                |= Parser.problem (Problem "uri")
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
    Parser.inContext ParsingAttrset <|
        Parser.sequence
            { start = token "{"
            , spaces = spaces
            , end = token "}"
            , trailing = Parser.Optional
            , separator = token ""
            , item = attribute
            }


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
        validFirst : Char -> Bool
        validFirst c =
            c == '_' || Char.isAlpha c

        validRest : Char -> Bool
        validRest c =
            c == '_' || c == '\'' || c == '-' || Char.isAlphaNum c
    in
    Parser.getChompedString
        (Parser.chompIf validFirst
            (Expecting "identifier's first character")
            |. Parser.chompWhile validRest
        )


list : Parser (List (Node Expression))
list =
    Parser.inContext ParsingList <|
        Parser.succeed identity
            |= Parser.problem (Problem "listParser")


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
                            "- "
                                ++ Ansi.Color.fontColor Ansi.Color.cyan (contextStackToString contextStack)
                                ++ ":\n  "
                                ++ String.join "\n  "
                                    (List.map problemToString problems)
                        )
                        multiple
                )


contextStackToString : List { row : Int, col : Int, context : Context } -> String
contextStackToString frames =
    frames
        |> List.reverse
        |> List.map (\{ row, col, context } -> contextToString context)
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


problemToString : Problem -> String
problemToString p =
    case p of
        Expecting s ->
            "expecting '" ++ s ++ "'"

        -- ExpectingInt ->
        --     "expecting int"
        -- ExpectingHex ->
        --     "expecting hex"
        -- ExpectingOctal ->
        --     "expecting octal"
        -- ExpectingBinary ->
        --     "expecting binary"
        -- ExpectingFloat ->
        --     "expecting float"
        -- ExpectingNumber ->
        --     "expecting number"
        -- ExpectingVariable ->
        --     "expecting variable"
        -- ExpectingSymbol s ->
        --     "expecting symbol '" ++ s ++ "'"
        -- ExpectingKeyword s ->
        --     "expecting keyword '" ++ s ++ "'"
        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        -- BadRepeat ->
        --     "bad repeat"
        Problem s ->
            "problem: " ++ s

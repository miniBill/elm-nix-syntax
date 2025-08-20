module Nix.Parser.Extra exposing (DeadEnd, Parser, errorToString)

import Ansi.Color
import List.Extra
import Nix.Parser.Context exposing (Context(..))
import Nix.Parser.Problem exposing (Problem(..))
import Parser.Advanced as Parser


type alias Parser a =
    Parser.Parser Context Problem a


type alias DeadEnd =
    Parser.DeadEnd Context Problem


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

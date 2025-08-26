module Parser.Error exposing
    ( renderError, Output, DeadEnd
    , Extract, forParser, forParserAdvanced, Expected(..)
    , problemToExpected
    )

{-|

@docs renderError, Output, DeadEnd


## Extraction

@docs Extract, forParser, forParserAdvanced, Expected


## Utilities

@docs problemToExpected

-}

import Json.Encode
import List.Extra
import Parser exposing (Problem(..))
import Set


{-| A type which encompasses both `Parser.DeadEnd` and `Parser.Advanced.DeadEnd`.
-}
type alias DeadEnd inner problem =
    { inner
        | row : Int
        , col : Int
        , problem : problem
    }


{-| Describes how to output the various parts of the error message.
-}
type alias Output out =
    { text : String -> out
    , formatCaret : out -> out
    , newline : out
    , formatContext : out -> out
    , linesOfExtraContext : Int
    }


{-| Describes how to get the context stack from a `DeadEnd` and how to extract expectations information from a problem.

You can usually use `forParser` and `forParserAdvanced`.

-}
type alias Extract inner problem =
    { contextStack :
        DeadEnd inner problem
        -> List { row : Int, col : Int, context : String }
    , problemToString : problem -> Expected
    }


{-| A problem is often of the form "expected `something`". This type is used to group those together.
-}
type Expected
    = Expected String
    | Other String


type Line a
    = Line (List a)


{-| And `Extract` for the basic `Parser.DeadEnd`.
-}
forParser : Extract {} Problem
forParser =
    { contextStack = \_ -> []
    , problemToString = problemToExpected
    }


{-| And `Extract` for `Parser.Advanced.DeadEnd` when the problem is a `Parser.Problem`.
-}
forParserAdvanced :
    Extract
        { contextStack : List { row : Int, col : Int, context : String }
        }
        Problem
forParserAdvanced =
    { contextStack = .contextStack
    , problemToString = problemToExpected
    }


{-| Render a list of `DeadEnd`s.

The `String` is the input to the parser.

This returns a list of "pieces", look at the README for examples of how to combine them.

-}
renderError :
    Output out
    -> Extract inner problem
    -> String
    -> List (DeadEnd inner problem)
    -> List out
renderError output extract src deadEnds =
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
        |> List.concatMap (\line -> deadEndToString output extract lines line)
        |> List.intersperse (Line [ output.newline ])
        |> List.concatMap (\(Line l) -> l)


deadEndToString :
    Output out
    -> Extract inner problem
    -> List ( Int, String )
    -> ( DeadEnd inner problem, List (DeadEnd inner problem) )
    -> List (Line out)
deadEndToString output extract lines ( head, tail ) =
    let
        grouped : List ( List { row : Int, col : Int, context : String }, List problem )
        grouped =
            (head :: tail)
                |> List.Extra.gatherEqualsBy extract.contextStack
                |> List.map
                    (\( groupHead, groupTail ) ->
                        ( extract.contextStack groupHead
                        , List.map .problem (groupHead :: groupTail)
                        )
                    )

        sourceFragment : List (Line out)
        sourceFragment =
            formatSourceFragment output { row = head.row, col = head.col } lines

        groupToString :
            ( List { row : Int, col : Int, context : String }, List problem )
            -> List (Line out)
        groupToString ( contextStack, problems ) =
            let
                ( expected, other ) =
                    List.foldl
                        (\problem ( expectedAcc, otherAcc ) ->
                            case extract.problemToString problem of
                                Expected e ->
                                    ( Set.insert e expectedAcc, otherAcc )

                                Other o ->
                                    ( expectedAcc, Set.insert o otherAcc )
                        )
                        ( Set.empty, Set.empty )
                        problems

                groupedExpected : List String
                groupedExpected =
                    case Set.toList expected of
                        [] ->
                            []

                        [ x ] ->
                            [ "Expecting " ++ x ]

                        (_ :: _ :: _) as l ->
                            [ "Expecting one of "
                                ++ String.join ", " l
                            ]

                problemsLines : List (Line out)
                problemsLines =
                    (groupedExpected ++ Set.toList other)
                        |> List.sort
                        |> List.map (\l -> Line [ output.text ("  " ++ l) ])
            in
            if List.isEmpty contextStack then
                problemsLines

            else
                Line
                    [ output.text "- "
                    , output.formatContext (output.text (contextStackToString contextStack))
                    , output.text ":"
                    ]
                    :: problemsLines
    in
    sourceFragment ++ Line [ output.text "" ] :: List.concatMap groupToString grouped


formatSourceFragment : Output a -> { row : Int, col : Int } -> List ( Int, String ) -> List (Line a)
formatSourceFragment output head lines =
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
                |> List.drop (head.row - output.linesOfExtraContext)
                |> List.take output.linesOfExtraContext
                |> List.Extra.takeWhile (\( i, _ ) -> i < head.row)

        after : List ( Int, String )
        after =
            lines
                |> List.drop head.row
                |> List.take output.linesOfExtraContext

        formatLine : ( Int, String ) -> Line a
        formatLine ( row, l ) =
            Line
                [ output.text
                    (String.padLeft numLength ' ' (String.fromInt row)
                        ++ "| "
                        ++ l
                    )
                ]

        numLength : Int
        numLength =
            after
                |> List.Extra.last
                |> Maybe.map (\( r, _ ) -> r)
                |> Maybe.withDefault head.row
                |> String.fromInt
                |> String.length

        caret : Line a
        caret =
            Line
                [ output.text (String.repeat (numLength + head.col + 1) " ")
                , output.formatCaret (output.text "^")
                ]
    in
    List.map formatLine before
        ++ formatLine line
        :: caret
        :: List.map formatLine after


contextStackToString : List { row : Int, col : Int, context : String } -> String
contextStackToString frames =
    frames
        |> List.reverse
        |> List.map
            (\{ row, col, context } ->
                context
                    ++ " ("
                    ++ String.fromInt row
                    ++ ":"
                    ++ String.fromInt col
                    ++ ")"
            )
        |> String.join " > "


{-| Categorize a `Problem` in whether it's "expected `something`" or something else.
-}
problemToExpected : Problem -> Expected
problemToExpected problem =
    case problem of
        Expecting x ->
            Expected (escape x)

        ExpectingVariable ->
            Expected "a variable"

        ExpectingEnd ->
            Expected "the end"

        ExpectingInt ->
            Expected "an integer"

        ExpectingHex ->
            Expected "an hexadecimal number"

        ExpectingOctal ->
            Expected "an octal number"

        ExpectingBinary ->
            Expected "a binary number"

        ExpectingFloat ->
            Expected "a floating point number"

        ExpectingNumber ->
            Expected "a number"

        ExpectingSymbol s ->
            Expected (escape s)

        ExpectingKeyword k ->
            Expected (escape k)

        UnexpectedChar ->
            Other "Unexpected char"

        Problem p ->
            Other p

        BadRepeat ->
            Other "Bad repetition"


escape : String -> String
escape x =
    Json.Encode.encode 0 (Json.Encode.string x)

module ParsingNixpkgs exposing (..)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cli.Option
import Cli.OptionsParser
import Cli.Program
import FatalError exposing (FatalError)
import List.Extra
import Nix.Parser
import Nix.Parser.Context exposing (Context(..))
import Nix.Parser.Problem exposing (Problem(..))
import Pages.Script as Script exposing (Script)
import Parser.Error


run : Script
run =
    Script.withCliOptions options script


type alias CliOptions =
    { path : String }


options : Cli.Program.Config CliOptions
options =
    Cli.Program.config
        |> Cli.Program.add
            (Cli.OptionsParser.build CliOptions
                |> Cli.OptionsParser.with
                    (Cli.Option.requiredPositionalArg "path")
            )


script : CliOptions -> BackendTask FatalError ()
script { path } =
    Do.do (files path) <| \list ->
    Do.log ("Found " ++ String.fromInt (List.length list) ++ " files") <| \() ->
    List.map tryParse list
        |> List.Extra.greedyGroupsOf 10
        |> List.map (\g -> g |> BackendTask.combine |> ignoreList)
        |> BackendTask.sequence
        |> ignoreList


ignoreList : BackendTask FatalError (List ()) -> BackendTask FatalError ()
ignoreList list =
    BackendTask.map (\_ -> ()) list


tryParse : String -> BackendTask FatalError ()
tryParse file =
    Do.allowFatal (File.rawFile file) <| \content ->
    case Nix.Parser.parse content of
        Ok _ ->
            Do.noop

        Err e ->
            let
                msg : String
                msg =
                    errorToString content e
            in
            BackendTask.fail (FatalError.fromString msg)


errorToString : String -> List Nix.Parser.DeadEnd -> String
errorToString src deadEnds =
    Parser.Error.renderError
        { text = identity
        , formatContext = Ansi.Color.fontColor Ansi.Color.cyan
        , formatCaret = Ansi.Color.fontColor Ansi.Color.red
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
            "let"

        ParsingApplication ->
            "application"

        ParsingPattern ->
            "pattern"


problemToExpected : Problem -> Parser.Error.Expected
problemToExpected problem =
    case problem of
        Expecting t ->
            Parser.Error.Expected t

        ExpectingEnd ->
            Parser.Error.Expected "the end"

        ExpectingVariable ->
            Parser.Error.Expected "a variable"

        ExpectingDigit ->
            Parser.Error.Expected "a digit"

        Unexpected t ->
            Parser.Error.Other t

        Unimplemented t ->
            Parser.Error.Other ("Unimplemented: " ++ t)


files : String -> BackendTask FatalError (List String)
files path =
    Glob.succeed identity
        |> Glob.match
            (Glob.literal
                (if String.endsWith "/" path then
                    path

                 else
                    path ++ "/"
                )
            )
        |> Glob.match Glob.recursiveWildcard
        |> Glob.match (Glob.literal "/")
        |> Glob.match Glob.wildcard
        |> Glob.match (Glob.literal ".nix")
        |> Glob.captureFilePath
        |> Glob.toBackendTask

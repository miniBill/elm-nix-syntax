module ParsingNixpkgs exposing (..)

import Ansi.Color
import BackendTask exposing (BackendTask)
import BackendTask.Custom as Custom
import BackendTask.Do as Do
import BackendTask.File as File
import BackendTask.Glob as Glob
import Cli.Option
import Cli.OptionsParser
import Cli.Program
import FatalError exposing (FatalError)
import Json.Decode
import Json.Encode
import List.Extra
import Nix.Parser
import Nix.Parser.Context exposing (Context(..))
import Nix.Parser.Extra
import Nix.Parser.Problem exposing (Problem(..))
import Pages.Script as Script exposing (Script)


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
    Do.allowFatal profile <| \() ->
    Do.do
        (List.map tryParse list
            |> safeCombine
        )
    <| \_ ->
    Do.allowFatal profileEnd <| \() ->
    Do.noop


safeCombine : List (BackendTask FatalError ()) -> BackendTask FatalError ()
safeCombine list =
    if List.length list < 10 then
        BackendTask.sequence list
            |> BackendTask.map (\_ -> ())

    else
        safeCombine
            (list
                |> List.Extra.greedyGroupsOf 10
                |> listSafeMap
                    (\g ->
                        g
                            |> BackendTask.sequence
                            |> BackendTask.map (\_ -> ())
                    )
            )


listSafeMap : (a -> b) -> List a -> List b
listSafeMap f l =
    let
        go : List a -> List b -> List b
        go queue acc =
            case queue of
                [] ->
                    List.reverse acc

                h :: t ->
                    go t (f h :: acc)
    in
    go l []


profile : BackendTask { fatal : FatalError, recoverable : Custom.Error } ()
profile =
    Custom.run "profile" Json.Encode.null (Json.Decode.succeed ())


profileEnd : BackendTask { fatal : FatalError, recoverable : Custom.Error } ()
profileEnd =
    Custom.run "profileEnd" Json.Encode.null (Json.Decode.succeed ())


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
                    Nix.Parser.Extra.errorToString
                        { cyan = Ansi.Color.fontColor Ansi.Color.cyan
                        , red = Ansi.Color.fontColor Ansi.Color.red
                        }
                        content
                        e
            in
            BackendTask.fail
                (FatalError.build
                    { title = "Parsing failed"
                    , body =
                        "Parsing failed in "
                            ++ file
                            ++ (case e of
                                    [] ->
                                        ""

                                    { row, col } :: _ ->
                                        ":" ++ String.fromInt row ++ ":" ++ String.fromInt col
                               )
                            ++ "\n\n"
                            ++ msg
                    }
                )


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

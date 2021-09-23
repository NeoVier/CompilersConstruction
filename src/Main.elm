{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


port module Main exposing (main)

{-| The main module is responsible for parsing command-line arguments,
requesting data from JS, and showing the output of our program
-}

import CCParser
import Emit.Program
import Emit.State
import Json.Decode as Decode
import Json.Encode as Encode
import Parser.Advanced as Parser
import Parser.Program as Program
import Syntax.Symbol



-- PORTS


port requestFile : String -> Cmd msg


port getFile : (Encode.Value -> msg) -> Sub msg


port print : String -> Cmd msg


port printTable : Encode.Value -> Cmd msg


port writeToFilePort : Encode.Value -> Cmd msg


writeToFile : { fileName : String, contents : String } -> Cmd msg
writeToFile { fileName, contents } =
    Encode.object
        [ ( "filename", Encode.string fileName )
        , ( "contents", Encode.string contents )
        ]
        |> writeToFilePort



-- MAIN


main : Program Encode.Value Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { outputFile : String
    }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        onlyPrintError err =
            ( { outputFile = "" }, print err )
    in
    case Decode.decodeValue (Decode.list Decode.string) flags of
        Ok [ inputFile, outputFile ] ->
            ( { outputFile = outputFile }
            , requestFile inputFile
            )

        Ok [] ->
            onlyPrintError "I received no file names! Please give me exactly two file names!"

        Ok [ _ ] ->
            onlyPrintError "I received too few arguments! I only accept an input file and an output file!"

        Ok _ ->
            onlyPrintError "I received too many arguments! I only accept an input file and an output file!"

        Err _ ->
            onlyPrintError "I couldn't decode the CLI arguments, make sure you're sending them in correctly!"



-- MSG


type Msg
    = GotFile (Result Decode.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotFile fileResult) model =
    case fileResult of
        Err err ->
            ( model
            , Decode.errorToString err
                |> print
            )

        Ok fileContent ->
            case Parser.run Program.program fileContent of
                Ok validProgram ->
                    case
                        Emit.Program.emit validProgram Emit.State.initialState
                            |> Emit.State.code
                    of
                        Ok validCode ->
                            ( model
                            , Cmd.batch
                                [ Syntax.Symbol.tableFromProgram validProgram
                                    |> Syntax.Symbol.encodeTable
                                    |> printTable
                                , writeToFile
                                    { fileName = model.outputFile
                                    , contents = validCode
                                    }
                                , [ "✓ All arithmetic expressions are valid"
                                  , "✓ All variable declarations are valid"
                                  , "✓ Every `break` statement is inside a `for` loop"
                                  , "✓ Every `return` statement is inside a function definition"
                                  ]
                                    |> String.join "\n"
                                    |> print
                                ]
                            )

                        Err err ->
                            ( model, print err )

                Err err ->
                    ( model
                    , CCParser.deadEndsToString err
                        |> print
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    getFile (Decode.decodeValue getFileDecoder >> GotFile)



-- HELPERS


getFileDecoder : Decode.Decoder String
getFileDecoder =
    Decode.field "fileContents" Decode.string

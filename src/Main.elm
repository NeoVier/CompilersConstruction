port module Main exposing (main)

import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Parser.Program as Program



-- PORTS


port requestFile : String -> Cmd msg


port getFile : (Encode.Value -> msg) -> Sub msg


port printError : String -> Cmd msg



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
    { fileContents : Maybe String }


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        filename =
            case Decode.decodeValue (Decode.list Decode.string) flags of
                Ok [ x ] ->
                    Ok x

                Ok [] ->
                    Err "I received no file names! Please give me exactly one file name!"

                Ok _ ->
                    Err "I received too many arguments! I only accept a single file name!"

                Err _ ->
                    Err "I couldn't decode the CLI arguments, make sure you're sending them in correctly!"
    in
    ( { fileContents = Nothing }
    , case filename of
        Ok file ->
            requestFile file

        Err err ->
            printError err
    )



-- MSG


type Msg
    = GotFile (Result Decode.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update (GotFile fileResult) _ =
    let
        _ =
            case fileResult of
                Ok fileContent ->
                    Parser.run Program.program fileContent
                        |> Debug.log "Result"

                Err _ ->
                    Err []
    in
    ( { fileContents = Result.toMaybe fileResult }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    getFile (Decode.decodeValue getFileDecoder >> GotFile)



-- HELPERS


getFileDecoder : Decode.Decoder String
getFileDecoder =
    Decode.field "fileContents" Decode.string

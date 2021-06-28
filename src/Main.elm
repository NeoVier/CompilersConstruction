port module Main exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Parser.Program as Program


port requestFile : String -> Cmd msg


port getFile : (Encode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { fileContents : Maybe String }


type Msg
    = GotFile (Result Decode.Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { fileContents = Nothing }, requestFile "examples/floatAttribution.lcc" )


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    getFile (Decode.decodeValue getFileDecoder >> GotFile)


getFileDecoder : Decode.Decoder String
getFileDecoder =
    Decode.field "fileContents" Decode.string

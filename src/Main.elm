port module Main exposing (..)


port printSomething : String -> Cmd msg


port getSomething : (String -> msg) -> Sub msg


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    ()


type Msg
    = GotSomething String


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), printSomething "Init" )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( (), printSomething "Update" )


subscriptions : Model -> Sub Msg
subscriptions () =
    getSomething GotSomething

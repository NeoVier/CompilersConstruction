{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


port module Main exposing (main)

{-| The main module is responsible for parsing command-line arguments,
requesting data from JS, and showing the output of our program
-}

import Json.Decode as Decode
import Json.Encode as Encode
import Parser
import Parser.Program as Program
import Set
import Syntax.Program
import Syntax.Symbol



-- PORTS


port requestFile : String -> Cmd msg


port getFile : (Encode.Value -> msg) -> Sub msg


port print : String -> Cmd msg


port printTable : Encode.Value -> Cmd msg



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
    { program : Maybe Syntax.Program.Program }


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
    ( { program = Nothing }
    , case filename of
        Ok file ->
            requestFile file

        Err err ->
            print err
    )



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
                    ( { model | program = Just validProgram }
                    , Cmd.batch
                        [ Syntax.Program.show validProgram
                            |> print
                        , Syntax.Symbol.tableFromProgram validProgram
                            |> Syntax.Symbol.encodeTable
                            |> printTable
                        ]
                    )

                Err err ->
                    ( model
                    , deadEndsToString err
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


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    String.join "\n"
        ("Something went wrong when parsing your source file. These are the errors I gathered:\n"
            :: (deadEnds
                    |> List.map (\deadEnd -> "\t" ++ deadEndToString deadEnd)
                    -- Because some parsers are backtrackable, we might get
                    -- duplicate errors, so we eliminate them here
                    |> Set.fromList
                    |> Set.toList
               )
        )
        ++ "\n"


deadEndToString : Parser.DeadEnd -> String
deadEndToString { row, col, problem } =
    let
        showPosition =
            "line " ++ String.fromInt row ++ ", column " ++ String.fromInt col
    in
    case problem of
        Parser.ExpectingInt ->
            "I was expecting to see an int on " ++ showPosition

        Parser.ExpectingHex ->
            "I was expecting to see a hexadecimal number on " ++ showPosition

        Parser.ExpectingOctal ->
            "I was expecting to see an octal number on " ++ showPosition

        Parser.ExpectingBinary ->
            "I was expecting to see a binary number on " ++ showPosition

        Parser.ExpectingFloat ->
            "I was expecting to see a float on " ++ showPosition

        Parser.ExpectingNumber ->
            "I was expecting to see a number on " ++ showPosition

        Parser.ExpectingVariable ->
            "I was expecting to see a variable on " ++ showPosition

        Parser.Expecting expected ->
            "I was expecting to see " ++ expected ++ " on " ++ showPosition

        Parser.ExpectingSymbol symbol ->
            "I was expecting to see the " ++ symbol ++ " symbol on " ++ showPosition

        Parser.ExpectingKeyword keyword ->
            "I was expecting to see the " ++ keyword ++ " keyword on " ++ showPosition

        Parser.ExpectingEnd ->
            "I was expecting to see the file to end on " ++ showPosition

        Parser.UnexpectedChar ->
            "I encountered an unexpected character on " ++ showPosition

        Parser.Problem problem_ ->
            "I got this problem: " ++ problem_ ++ " on " ++ showPosition

        Parser.BadRepeat ->
            "I got a bad repeat error on " ++ showPosition

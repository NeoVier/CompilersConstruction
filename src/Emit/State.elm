{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.State exposing
    ( Context(..)
    , Label
    , State
    , VariableType(..)
    , addLabel
    , addLine
    , addLineWithLabel
    , addTemporaryVariable
    , addVariable
    , code
    , createLabel
    , currentContext
    , enterContext
    , initialState
    , lastTemporaryVariable
    , leaveContext
    , raiseError
    )

import Dict exposing (Dict)


type State
    = Valid
        { code : List String
        , lastTemporaryIndex : Int
        , variables : Dict String VariableType
        , lastLabelIndex : Int
        , contexts : List Context
        }
    | WithError


initialState : State
initialState =
    Valid
        { code = []
        , lastTemporaryIndex = -1
        , variables = Dict.empty
        , lastLabelIndex = -1
        , contexts = []
        }


raiseError : State -> State
raiseError _ =
    WithError



-- CODE MANIPULATION


code : State -> String
code state =
    case state of
        Valid state_ ->
            state_.code
                |> List.reverse
                |> String.join "\n"

        WithError ->
            ""


addLine : String -> State -> State
addLine line state =
    case state of
        Valid state_ ->
            Valid { state_ | code = line :: state_.code }

        WithError ->
            WithError



-- CONTEXT


type Context
    = ForContext Label
    | IfContext
    | ElseContext
    | FunctionContext Label
    | BlockContext


enterContext : Context -> State -> State
enterContext context state =
    case state of
        Valid state_ ->
            Valid { state_ | contexts = context :: state_.contexts }

        WithError ->
            WithError


leaveContext : State -> State
leaveContext state =
    case state of
        Valid state_ ->
            Valid { state_ | contexts = List.drop 1 state_.contexts }

        WithError ->
            WithError


currentContext : State -> Maybe Context
currentContext state =
    case state of
        Valid state_ ->
            List.head state_.contexts

        WithError ->
            Nothing


lastTemporaryVariable : State -> String
lastTemporaryVariable state =
    case state of
        Valid state_ ->
            temporaryVariable state_.lastTemporaryIndex

        WithError ->
            temporaryVariable -2


addTemporaryVariable : { type_ : VariableType, attribution : String } -> State -> State
addTemporaryVariable { type_, attribution } state =
    case state of
        Valid state_ ->
            let
                tempName =
                    temporaryVariable (state_.lastTemporaryIndex + 1)

                attributionCode : String
                attributionCode =
                    "{{TEMP}} = {{ATTRIBUTION}}"
                        |> String.replace "{{TEMP}}" tempName
                        |> String.replace "{{ATTRIBUTION}}" attribution
            in
            Valid
                { state_
                    | code = attributionCode :: state_.code
                    , lastTemporaryIndex = state_.lastTemporaryIndex + 1
                    , variables = Dict.insert tempName type_ state_.variables
                }

        WithError ->
            WithError



-- LABELS


type Label
    = Label String


addVariable : { type_ : VariableType, name : String } -> State -> State
addVariable { type_, name } state =
    case state of
        Valid state_ ->
            -- TODO - Accept dimmensions, check context, check if exists
            Valid { state_ | variables = Dict.insert name type_ state_.variables }

        WithError ->
            WithError


createLabel : State -> ( State, Label )
createLabel state =
    case state of
        Valid state_ ->
            let
                label : Int -> Label
                label index =
                    Label ("$label$" ++ String.fromInt index)
            in
            ( Valid { state_ | lastLabelIndex = state_.lastLabelIndex + 1 }
            , label (state_.lastLabelIndex + 1)
            )

        WithError ->
            ( WithError, Label "INVALID_LABEL" )


addLineWithLabel : { code : String, labelPlaceholder : String } -> Label -> State -> State
addLineWithLabel codeInfo (Label labelValue) state =
    case state of
        Valid state_ ->
            let
                newCode =
                    codeInfo.code
                        |> String.replace codeInfo.labelPlaceholder labelValue
            in
            Valid { state_ | code = newCode :: state_.code }

        WithError ->
            WithError


addLabel : Label -> State -> State
addLabel (Label labelValue) state =
    case state of
        Valid state_ ->
            let
                newCode =
                    labelValue ++ ":"
            in
            Valid { state_ | code = newCode :: state_.code }

        WithError ->
            WithError



-- VARIABLES


type VariableType
    = IntVariable
    | FloatVariable
    | StringVariable
      -- Used for placeholder for now
    | InvalidType


temporaryVariable : Int -> String
temporaryVariable index =
    "$temp$" ++ String.fromInt index

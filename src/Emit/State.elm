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
    , areSameType
    , code
    , createLabel
    , currentContext
    , enterContext
    , getVariableType
    , initialState
    , lastTemporaryVariable
    , leaveContext
    , raiseError
    , typeToString
    )

import Dict exposing (Dict)


type State
    = Valid
        { code : List String
        , lastTemporaryIndex : Int
        , variables : Dict String Variable
        , lastLabelIndex : Int
        , lastContextIndex : Int
        , contexts : List TaggedContext
        }
    | WithError Error


type alias Variable =
    { variableType : VariableType, definedInContext : Int }


type alias Error =
    { range : { start : ( Int, Int ), end : ( Int, Int ) }
    , message : String
    }


initialState : State
initialState =
    Valid
        { code = []
        , lastTemporaryIndex = -1
        , variables = Dict.empty
        , lastLabelIndex = -1
        , lastContextIndex = -1
        , contexts = []
        }


raiseError : Error -> State -> State
raiseError error state =
    case state of
        Valid _ ->
            WithError error

        WithError err ->
            WithError err



-- CODE MANIPULATION


code : State -> String
code state =
    case state of
        Valid state_ ->
            state_.code
                |> List.reverse
                |> String.join "\n"

        WithError error ->
            let
                showLineCol ( line, col ) =
                    String.fromInt line ++ ":" ++ String.fromInt col
            in
            "Error from {{START}} to {{END}}:\n\t{{MESSAGE}}"
                |> String.replace "{{START}}" (showLineCol error.range.start)
                |> String.replace "{{END}}" (showLineCol error.range.end)
                |> String.replace "{{MESSAGE}}" error.message


addLine : String -> State -> State
addLine line state =
    case state of
        Valid state_ ->
            Valid { state_ | code = line :: state_.code }

        WithError err ->
            WithError err



-- CONTEXT


type Context
    = ForContext Label
    | IfContext
    | ElseContext
    | FunctionContext Label
    | BlockContext


type TaggedContext
    = TaggedForContext Label Int
    | TaggedIfContext Int
    | TaggedElseContext Int
    | TaggedFunctionContext Label Int
    | TaggedBlockContext Int


tagContext : Context -> Int -> TaggedContext
tagContext context tag =
    case context of
        ForContext label ->
            TaggedForContext label tag

        IfContext ->
            TaggedIfContext tag

        ElseContext ->
            TaggedElseContext tag

        FunctionContext label ->
            TaggedFunctionContext label tag

        BlockContext ->
            TaggedBlockContext tag


untagContext : TaggedContext -> Context
untagContext context =
    case context of
        TaggedForContext label _ ->
            ForContext label

        TaggedIfContext _ ->
            IfContext

        TaggedElseContext _ ->
            ElseContext

        TaggedFunctionContext label _ ->
            FunctionContext label

        TaggedBlockContext _ ->
            BlockContext


enterContext : Context -> State -> State
enterContext context state =
    case state of
        Valid state_ ->
            Valid
                { state_
                    | contexts =
                        tagContext context (state_.lastContextIndex + 1) :: state_.contexts
                    , lastContextIndex = state_.lastContextIndex + 1
                }

        WithError err ->
            WithError err


leaveContext : State -> State
leaveContext state =
    case state of
        Valid state_ ->
            { state_
                | contexts = List.drop 1 state_.contexts
                , lastContextIndex = state_.lastContextIndex - 1
                , variables =
                    Dict.filter
                        (\_ { definedInContext } ->
                            definedInContext < state_.lastContextIndex
                        )
                        state_.variables
            }
                |> Valid

        WithError err ->
            WithError err


currentContext : State -> Maybe Context
currentContext state =
    case state of
        Valid state_ ->
            List.head state_.contexts
                |> Maybe.map untagContext

        WithError _ ->
            Nothing



-- LABELS


type Label
    = Label String


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

        WithError err ->
            ( WithError err, Label "INVALID_LABEL" )


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

        WithError err ->
            WithError err


addLabel : Label -> State -> State
addLabel (Label labelValue) state =
    case state of
        Valid state_ ->
            let
                newCode =
                    labelValue ++ ":"
            in
            Valid { state_ | code = newCode :: state_.code }

        WithError err ->
            WithError err



-- VARIABLES


type VariableType
    = IntVariable
    | FloatVariable
    | StringVariable
    | NullVariable
      -- Used for placeholder for now
    | InvalidType


typeToString : VariableType -> String
typeToString variableType =
    case variableType of
        IntVariable ->
            "Int"

        FloatVariable ->
            "Float"

        StringVariable ->
            "String"

        NullVariable ->
            "Null"

        InvalidType ->
            "INVALID_TYPE"


getVariableType : State -> String -> Maybe VariableType
getVariableType state variable =
    case state of
        Valid state_ ->
            getVariableByName variable state_.variables
                |> Maybe.map (Tuple.second >> .variableType)

        WithError _ ->
            Nothing


temporaryVariable : Int -> String
temporaryVariable index =
    "$temp$" ++ String.fromInt index


addVariable : { type_ : VariableType, name : String } -> State -> State
addVariable { type_, name } state =
    case state of
        Valid state_ ->
            -- TODO - Accept dimmensions, check context, check if exists
            Valid
                { state_
                    | variables =
                        Dict.insert (variableNameWithContext name state_.lastContextIndex)
                            { variableType = type_
                            , definedInContext = state_.lastContextIndex
                            }
                            state_.variables
                }

        WithError err ->
            WithError err


lastTemporaryVariable : State -> String
lastTemporaryVariable state =
    case state of
        Valid state_ ->
            temporaryVariable state_.lastTemporaryIndex

        WithError _ ->
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
                    , variables =
                        Dict.insert (variableNameWithContext tempName state_.lastContextIndex)
                            { variableType = type_
                            , definedInContext = state_.lastContextIndex
                            }
                            state_.variables
                }

        WithError err ->
            WithError err


variableNameWithContext : String -> Int -> String
variableNameWithContext name contextIndex =
    "$context${{INDEX}}${{NAME}}"
        |> String.replace "{{INDEX}}" (String.fromInt contextIndex)
        |> String.replace "{{NAME}}" name


getVariableByName : String -> Dict String Variable -> Maybe ( String, Variable )
getVariableByName name variables =
    let
        nameEnd =
            "${{NAME}}"
                |> String.replace "{{NAME}}" name
    in
    Dict.filter (\nameKey _ -> String.endsWith nameEnd nameKey) variables
        |> Dict.toList
        |> List.sortBy (\( _, { definedInContext } ) -> definedInContext)
        |> List.reverse
        |> List.head


areSameType : String -> String -> State -> Bool
areSameType firstVar secondVar state =
    case state of
        Valid state_ ->
            case
                ( getVariableByName firstVar state_.variables
                , getVariableByName secondVar state_.variables
                )
            of
                ( Just ( _, firstType ), Just ( _, secondType ) ) ->
                    (firstType.variableType == NullVariable)
                        || (secondType.variableType == NullVariable)
                        || (firstType == secondType)

                _ ->
                    False

        WithError _ ->
            False

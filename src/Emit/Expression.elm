{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.Expression exposing (emit, fromVariableAccessor)

import Emit.State as State exposing (State)
import List exposing (range)
import Syntax.Expression



-- TODO - Set correct types


emit : Syntax.Expression.Expression -> State -> State
emit expression state =
    case expression of
        Syntax.Expression.SingleExpression numericalExpression _ ->
            fromNumericalExpression numericalExpression state

        Syntax.Expression.WithComparator firstExpression comparator secondExpression range ->
            let
                afterFirst =
                    fromNumericalExpression firstExpression state

                afterSecond =
                    fromNumericalExpression secondExpression afterFirst

                comparatorString =
                    case comparator of
                        Syntax.Expression.LessThan ->
                            "<"

                        Syntax.Expression.Greater ->
                            ">"

                        Syntax.Expression.LessThanOrEqualTo ->
                            "<="

                        Syntax.Expression.GreaterThanOrEqualTo ->
                            ">="

                        Syntax.Expression.Equals ->
                            "=="

                        Syntax.Expression.Different ->
                            "!="

                attribution =
                    "{{FIRST}} {{COMPARATOR}} {{SECOND}}"
                        |> String.replace "{{FIRST}}" (State.lastTemporaryVariable afterFirst)
                        |> String.replace "{{COMPARATOR}}" comparatorString
                        |> String.replace "{{SECOND}}" (State.lastTemporaryVariable afterSecond)
            in
            if
                State.areSameType (State.lastTemporaryVariable afterFirst)
                    (State.lastTemporaryVariable afterSecond)
                    afterSecond
            then
                State.addTemporaryVariable
                    { type_ = State.IntVariable
                    , attribution = attribution
                    }
                    afterSecond

            else
                State.raiseError
                    { range = range
                    , message =
                        "I was trying to compare two values using `{{COMPARATOR}}`. The value on the left was a {{FIRST_VAR}}, and the one on the right was a {{SECOND_VAR}}, but I need them to be of the same type!"
                            |> String.replace "{{COMPARATOR}}" comparatorString
                            |> String.replace "{{FIRST_VAR}}"
                                (State.lastTemporaryVariable afterFirst
                                    |> State.getVariableType afterSecond
                                    |> Maybe.map State.typeToString
                                    |> Maybe.withDefault "unknown type"
                                )
                            |> String.replace "{{SECOND_VAR}}"
                                (State.lastTemporaryVariable afterSecond
                                    |> State.getVariableType afterSecond
                                    |> Maybe.map State.typeToString
                                    |> Maybe.withDefault "unknown type"
                                )
                    }
                    afterSecond


fromVariableAccessor : Syntax.Expression.VariableAccessor -> State -> ( State, String )
fromVariableAccessor variableAccessor state =
    let
        -- TODO - Check dimmensions
        ( afterAccessors, accessorVariables ) =
            List.foldl
                (\currAccessor ( currState, currVariables ) ->
                    let
                        afterAccessor =
                            fromNumericalExpression currAccessor currState
                    in
                    ( afterAccessor
                    , State.lastTemporaryVariable afterAccessor :: currVariables
                    )
                )
                ( state, [] )
                variableAccessor.accessors

        accessorsString =
            accessorVariables
                |> List.reverse
                |> List.map (\accessor -> "[" ++ accessor ++ "]")
                |> String.concat
    in
    ( afterAccessors, variableAccessor.name ++ accessorsString )


fromNumericalExpression : Syntax.Expression.NumericalExpression -> State -> State
fromNumericalExpression numericalExpression state =
    case numericalExpression of
        Syntax.Expression.SingleNumericalExpression term _ ->
            fromTerm term state

        Syntax.Expression.MultipleNumericalExpressions term numericalOperator otherExpression range ->
            let
                afterTerm =
                    fromTerm term state

                afterOtherExpression =
                    fromNumericalExpression otherExpression afterTerm

                ( operatorString, operation ) =
                    case numericalOperator of
                        Syntax.Expression.Addition ->
                            ( "+", "add" )

                        Syntax.Expression.Subtraction ->
                            ( "-", "subtract" )

                attribution =
                    "{{TERM}} {{OPERATOR}} {{NUMEXPR}}"
                        |> String.replace "{{TERM}}" (State.lastTemporaryVariable afterTerm)
                        |> String.replace "{{OPERATOR}}" operatorString
                        |> String.replace "{{NUMEXPR}}" (State.lastTemporaryVariable afterOtherExpression)
            in
            if
                State.areSameType (State.lastTemporaryVariable afterTerm)
                    (State.lastTemporaryVariable afterOtherExpression)
                    afterOtherExpression
            then
                case
                    State.lastTemporaryVariable afterOtherExpression
                        |> State.getVariableType afterOtherExpression
                of
                    Nothing ->
                        State.raiseError
                            { range = range
                            , message =
                                "I was trying to {{OPERATION}} two values (using {{OPERATOR}}). The two values had the same type, but somehow I couldn't figure out what type that is."
                                    |> String.replace "{{OPERATION}}" operation
                                    |> String.replace "{{OPERATOR}}" operatorString
                            }
                            afterOtherExpression

                    Just variableType ->
                        State.addTemporaryVariable
                            { type_ = variableType
                            , attribution = attribution
                            }
                            afterOtherExpression

            else
                State.raiseError
                    { range = range
                    , message =
                        "I was trying to {{OPERATION}} two values (using {{OPERATOR}}). The value on the left was a {{FIRST_VAL}}, and the one on the right was a {{SECOND_VAL}}, but I need them to be of the same type!"
                            |> String.replace "{{OPERATION}}" operation
                            |> String.replace "{{OPERATOR}}" operatorString
                            |> String.replace "{{FIRST_VAL}}"
                                (State.lastTemporaryVariable afterTerm
                                    |> State.getVariableType afterOtherExpression
                                    |> Maybe.map State.typeToString
                                    |> Maybe.withDefault "unknown type"
                                )
                            |> String.replace "{{SECOND_VAL}}"
                                (State.lastTemporaryVariable afterOtherExpression
                                    |> State.getVariableType afterOtherExpression
                                    |> Maybe.map State.typeToString
                                    |> Maybe.withDefault "unknown type"
                                )
                    }
                    afterOtherExpression


fromTerm : Syntax.Expression.Term -> State -> State
fromTerm term state =
    case term of
        Syntax.Expression.SingleTerm unaryExpression _ ->
            fromUnaryExpression unaryExpression state

        Syntax.Expression.MultipleTerms unaryExpression termOperator otherTerm range ->
            let
                afterUnaryExpression =
                    fromUnaryExpression unaryExpression state

                afterOtherTerm =
                    fromTerm otherTerm afterUnaryExpression

                ( operatorString, operation ) =
                    case termOperator of
                        Syntax.Expression.Multiplication ->
                            ( "*", "multiply" )

                        Syntax.Expression.Division ->
                            ( "/", "divide" )

                        Syntax.Expression.Modulo ->
                            ( "%", "apply modulo" )

                attribution =
                    "{{UNARYEXPR}} {{OPERATOR}} {{TERM}}"
                        |> String.replace "{{UNARYEXPR}}" (State.lastTemporaryVariable afterUnaryExpression)
                        |> String.replace "{{OPERATOR}}" operatorString
                        |> String.replace "{{TERM}}" (State.lastTemporaryVariable afterOtherTerm)
            in
            if
                State.areSameType (State.lastTemporaryVariable afterUnaryExpression)
                    (State.lastTemporaryVariable afterOtherTerm)
                    afterOtherTerm
            then
                case
                    State.lastTemporaryVariable afterOtherTerm
                        |> State.getVariableType afterOtherTerm
                of
                    Nothing ->
                        State.raiseError
                            { range = range
                            , message =
                                "I was trying to {{OPERATION}} two values (using {{OPERATOR}}). The two values had the same type, but somehow I couldn't figure out what type that is."
                                    |> String.replace "{{OPERATION}}" operation
                                    |> String.replace "{{OPERATOR}}" operatorString
                            }
                            afterOtherTerm

                    Just variableType ->
                        State.addTemporaryVariable
                            { type_ = variableType, attribution = attribution }
                            afterOtherTerm

            else
                State.raiseError
                    { range = range
                    , message =
                        "I was trying to {{OPERATION}} two values (using {{OPERATOR}}). The value on the left was a {{FIRST_VAL}}, and the one on the right was a {{SECOND_VAL}}, but I need them to be of the same type!"
                            |> String.replace "{{OPERATION}}" operation
                            |> String.replace "{{OPERATOR}}" operatorString
                            |> String.replace "{{FIRST_VAL}}"
                                (State.lastTemporaryVariable afterUnaryExpression
                                    |> State.getVariableType afterOtherTerm
                                    |> Maybe.map State.typeToString
                                    |> Maybe.withDefault "unknown type"
                                )
                            |> String.replace "{{SECOND_VAL}}"
                                (State.lastTemporaryVariable afterOtherTerm
                                    |> State.getVariableType afterOtherTerm
                                    |> Maybe.map State.typeToString
                                    |> Maybe.withDefault "unknown type"
                                )
                    }
                    afterOtherTerm


fromUnaryExpression : Syntax.Expression.UnaryExpression -> State -> State
fromUnaryExpression (Syntax.Expression.UnaryExpression maybeSign factor range) state =
    case fromFactor factor state of
        SimpleFactor factorCode factorType ->
            let
                attribution : Bool -> String
                attribution addMinusSign =
                    "{{MINUS_SIGN}}{{FACTOR_CODE}}"
                        |> String.replace "{{MINUS_SIGN}}"
                            (if addMinusSign then
                                "-1 * "

                             else
                                ""
                            )
                        |> String.replace "{{FACTOR_CODE}}" factorCode

                isMinus =
                    case maybeSign of
                        Nothing ->
                            False

                        Just Syntax.Expression.Plus ->
                            False

                        Just Syntax.Expression.Minus ->
                            True

                isString =
                    case factorType of
                        State.StringVariable ->
                            True

                        _ ->
                            False
            in
            if isString && isMinus then
                State.raiseError
                    { range = range
                    , message = "I can't have a negative `String`! Does that even make sense?"
                    }
                    state

            else
                State.addTemporaryVariable
                    { type_ = factorType, attribution = attribution isMinus }
                    state

        ComplexFactor afterFactor ->
            case maybeSign of
                Nothing ->
                    afterFactor

                Just Syntax.Expression.Plus ->
                    afterFactor

                Just Syntax.Expression.Minus ->
                    case
                        State.lastTemporaryVariable afterFactor
                            |> State.getVariableType afterFactor
                    of
                        Nothing ->
                            State.raiseError
                                { range = range
                                , message = "Somehow I couldn't figure out the type of this expression"
                                }
                                afterFactor

                        Just State.StringVariable ->
                            State.raiseError
                                { range = range
                                , message = "I can't have a negative `String`! Does that even make sense?"
                                }
                                afterFactor

                        Just variableType ->
                            let
                                attribution =
                                    "-1 * {{FACTOR}}"
                                        |> String.replace "{{FACTOR}}" (State.lastTemporaryVariable afterFactor)
                            in
                            State.addTemporaryVariable
                                { type_ = variableType, attribution = attribution }
                                afterFactor


type FactorResult
    = SimpleFactor String State.VariableType
    | ComplexFactor State


fromFactor : Syntax.Expression.Factor -> State -> FactorResult
fromFactor factor state =
    case factor of
        Syntax.Expression.IntFactor value ->
            SimpleFactor (String.fromInt value) State.IntVariable

        Syntax.Expression.FloatFactor value ->
            SimpleFactor (String.fromFloat value) State.FloatVariable

        Syntax.Expression.StringFactor value ->
            SimpleFactor ("\"" ++ value ++ "\"") State.StringVariable

        Syntax.Expression.NullFactor ->
            SimpleFactor "null" State.NullVariable

        Syntax.Expression.NamedFactor accessors range ->
            case accessors.accessors of
                [] ->
                    case State.getVariableType state accessors.name of
                        Nothing ->
                            State.raiseError
                                { range = range
                                , message =
                                    "Somehow I couldn't figure out the type of {{NAME}}. Have you declared it already?"
                                        |> String.replace "{{NAME}}" accessors.name
                                }
                                state
                                |> ComplexFactor

                        Just variableType ->
                            state
                                |> State.addTemporaryVariable
                                    { type_ = variableType
                                    , attribution = accessors.name
                                    }
                                |> ComplexFactor

                first :: rest ->
                    let
                        afterFirstExpression : State
                        afterFirstExpression =
                            fromNumericalExpression first state

                        attributionAfterFirstExpression : String
                        attributionAfterFirstExpression =
                            "{{NAME}}[{{NUMEXPR}}]"
                                |> String.replace "{{NAME}}" accessors.name
                                |> String.replace "{{NUMEXPR}}" (State.lastTemporaryVariable afterFirstExpression)

                        afterFirst : State
                        afterFirst =
                            -- TODO - Check types
                            State.addTemporaryVariable
                                { type_ = State.InvalidType, attribution = attributionAfterFirstExpression }
                                afterFirstExpression

                        afterRest : State
                        afterRest =
                            List.foldl
                                (\currAccessor currState ->
                                    let
                                        afterExpression : State
                                        afterExpression =
                                            fromNumericalExpression currAccessor currState

                                        attribution =
                                            "{{LAST_TEMP}}[{{NUMEXPR}}]"
                                                |> String.replace "{{LAST_TEMP}}" (State.lastTemporaryVariable currState)
                                                |> String.replace "{{NUMEXPR}}" (State.lastTemporaryVariable afterExpression)
                                    in
                                    State.addTemporaryVariable
                                        { type_ = State.InvalidType, attribution = attribution }
                                        afterExpression
                                )
                                afterFirst
                                rest
                    in
                    ComplexFactor afterRest

        Syntax.Expression.ParenthesizedFactor expression ->
            fromNumericalExpression expression state
                |> ComplexFactor

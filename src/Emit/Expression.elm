{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.Expression exposing (emit, fromVariableAccessor)

import Emit.State as State exposing (State)
import Syntax.Expression



-- TODO - Set correct types


emit : Syntax.Expression.Expression -> State -> State
emit expression state =
    case expression of
        Syntax.Expression.SingleExpression numericalExpression ->
            fromNumericalExpression numericalExpression state

        Syntax.Expression.WithComparator firstExpression comparator secondExpression ->
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
            State.addTemporaryVariable
                { type_ = State.InvalidType
                , attribution = attribution
                }
                afterSecond


fromVariableAccessor : Syntax.Expression.VariableAccessor -> State -> ( State, String )
fromVariableAccessor variableAccessor state =
    let
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
        Syntax.Expression.SingleNumericalExpression term ->
            fromTerm term state

        Syntax.Expression.MultipleNumericalExpressions term numericalOperator otherExpression ->
            let
                afterTerm =
                    fromTerm term state

                afterOtherExpression =
                    fromNumericalExpression otherExpression afterTerm

                operatorString =
                    case numericalOperator of
                        Syntax.Expression.Addition ->
                            "+"

                        Syntax.Expression.Subtraction ->
                            "-"

                attribution =
                    "{{TERM}} {{OPERATOR}} {{NUMEXPR}}"
                        |> String.replace "{{TERM}}" (State.lastTemporaryVariable afterTerm)
                        |> String.replace "{{OPERATOR}}" operatorString
                        |> String.replace "{{NUMEXPR}}" (State.lastTemporaryVariable afterOtherExpression)
            in
            State.addTemporaryVariable
                { type_ = State.InvalidType, attribution = attribution }
                afterOtherExpression


fromTerm : Syntax.Expression.Term -> State -> State
fromTerm term state =
    case term of
        Syntax.Expression.SingleTerm unaryExpression ->
            fromUnaryExpression unaryExpression state

        Syntax.Expression.MultipleTerms unaryExpression termOperator otherTerm ->
            let
                afterUnaryExpression =
                    fromUnaryExpression unaryExpression state

                afterOtherTerm =
                    fromTerm otherTerm afterUnaryExpression

                operatorString =
                    case termOperator of
                        Syntax.Expression.Multiplication ->
                            "*"

                        Syntax.Expression.Division ->
                            "/"

                        Syntax.Expression.Modulo ->
                            "%"

                attribution =
                    "{{UNARYEXPR}} {{OPERATOR}} {{TERM}}"
                        |> String.replace "{{UNARYEXPR}}" (State.lastTemporaryVariable afterUnaryExpression)
                        |> String.replace "{{OPERATOR}}" operatorString
                        |> String.replace "{{TERM}}" (State.lastTemporaryVariable afterOtherTerm)
            in
            State.addTemporaryVariable
                { type_ = State.InvalidType, attribution = attribution }
                afterOtherTerm


fromUnaryExpression : Syntax.Expression.UnaryExpression -> State -> State
fromUnaryExpression (Syntax.Expression.UnaryExpression maybeSign factor) state =
    case fromFactor factor state of
        SimpleFactor factorCode ->
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
            in
            State.addTemporaryVariable
                { type_ = State.InvalidType, attribution = attribution isMinus }
                state

        ComplexFactor afterFactor ->
            case maybeSign of
                Nothing ->
                    afterFactor

                Just Syntax.Expression.Plus ->
                    afterFactor

                Just Syntax.Expression.Minus ->
                    let
                        attribution =
                            "-1 * {{FACTOR}}"
                                |> String.replace "{{FACTOR}}" (State.lastTemporaryVariable afterFactor)
                    in
                    State.addTemporaryVariable
                        { type_ = State.InvalidType, attribution = attribution }
                        afterFactor


type FactorResult
    = SimpleFactor String
    | ComplexFactor State


fromFactor : Syntax.Expression.Factor -> State -> FactorResult
fromFactor factor state =
    case factor of
        Syntax.Expression.IntFactor value ->
            SimpleFactor (String.fromInt value)

        Syntax.Expression.FloatFactor value ->
            SimpleFactor (String.fromFloat value)

        Syntax.Expression.StringFactor value ->
            SimpleFactor ("\"" ++ value ++ "\"")

        Syntax.Expression.NullFactor ->
            SimpleFactor "null"

        Syntax.Expression.NamedFactor accessors ->
            case accessors.accessors of
                [] ->
                    state
                        |> State.addTemporaryVariable
                            { type_ = State.InvalidType, attribution = accessors.name }
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

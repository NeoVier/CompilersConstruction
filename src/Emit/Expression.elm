{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.Expression exposing (emit)

import Emit.State exposing (State, temporaryVariable)
import Syntax.Expression


emit : Syntax.Expression.Expression -> State -> State
emit expression state =
    case expression of
        Syntax.Expression.SingleExpression numericalExpression ->
            fromNumericalExpression numericalExpression state

        Syntax.Expression.WithComparator firstExpression comparator secondExpression ->
            let
                afterFirst =
                    fromNumericalExpression firstExpression state

                firstLastVariableIndex =
                    afterFirst.lastTemporaryIndex

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

                newCode =
                    "{{TEMP}} = {{FIRST}} {{COMPARATOR}} {{SECOND}}"
                        |> String.replace "{{TEMP}}" (temporaryVariable (afterSecond.lastTemporaryIndex + 1))
                        |> String.replace "{{FIRST}}" (temporaryVariable firstLastVariableIndex)
                        |> String.replace "{{COMPARATOR}}" comparatorString
                        |> String.replace "{{SECOND}}" (temporaryVariable afterSecond.lastTemporaryIndex)
            in
            { afterSecond
                | code = newCode :: afterSecond.code
                , lastTemporaryIndex = afterSecond.lastTemporaryIndex + 1
            }


fromNumericalExpression : Syntax.Expression.NumericalExpression -> State -> State
fromNumericalExpression numericalExpression state =
    case numericalExpression of
        Syntax.Expression.SingleNumericalExpression term ->
            fromTerm term state

        Syntax.Expression.MultipleNumericalExpressions term numericalOperator otherExpression ->
            let
                afterTerm =
                    fromTerm term state

                termLastVariableIndex =
                    afterTerm.lastTemporaryIndex

                afterOtherExpression =
                    fromNumericalExpression otherExpression afterTerm

                operatorString =
                    case numericalOperator of
                        Syntax.Expression.Addition ->
                            "+"

                        Syntax.Expression.Subtraction ->
                            "-"

                newCode =
                    "{{TEMP}} = {{TERM}} {{OPERATOR}} {{NUMEXPR}}"
                        |> String.replace "{{TEMP}}" (temporaryVariable (afterOtherExpression.lastTemporaryIndex + 1))
                        |> String.replace "{{TERM}}" (temporaryVariable termLastVariableIndex)
                        |> String.replace "{{OPERATOR}}" operatorString
                        |> String.replace "{{NUMEXPR}}" (temporaryVariable afterOtherExpression.lastTemporaryIndex)
            in
            { afterOtherExpression
                | code = newCode :: afterOtherExpression.code
                , lastTemporaryIndex = afterOtherExpression.lastTemporaryIndex + 1
            }


fromTerm : Syntax.Expression.Term -> State -> State
fromTerm term state =
    case term of
        Syntax.Expression.SingleTerm unaryExpression ->
            fromUnaryExpression unaryExpression state

        Syntax.Expression.MultipleTerms unaryExpression termOperator otherTerm ->
            let
                afterUnaryExpression =
                    fromUnaryExpression unaryExpression state

                unaryExpressionLastVariableIndex =
                    afterUnaryExpression.lastTemporaryIndex

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

                newCode =
                    "{{TEMP}} = {{UNARYEXPR}} {{OPERATOR}} {{TERM}}"
                        |> String.replace "{{TEMP}}" (temporaryVariable (afterOtherTerm.lastTemporaryIndex + 1))
                        |> String.replace "{{UNARYEXPR}}" (temporaryVariable unaryExpressionLastVariableIndex)
                        |> String.replace "{{OPERATOR}}" operatorString
                        |> String.replace "{{TERM}}" (temporaryVariable afterOtherTerm.lastTemporaryIndex)
            in
            { afterOtherTerm
                | code = newCode :: afterOtherTerm.code
                , lastTemporaryIndex = afterOtherTerm.lastTemporaryIndex + 1
            }


fromUnaryExpression : Syntax.Expression.UnaryExpression -> State -> State
fromUnaryExpression (Syntax.Expression.UnaryExpression maybeSign factor) state =
    case fromFactor factor state of
        SimpleFactor factorCode ->
            let
                newCode : Bool -> String
                newCode addMinusSign =
                    "{{TEMP}} = {{MINUS_SIGN}}{{FACTOR_CODE}}"
                        |> String.replace "{{TEMP}}" (temporaryVariable (state.lastTemporaryIndex + 1))
                        |> String.replace "{{MINUS_SIGN}}"
                            (if addMinusSign then
                                "-1 * "

                             else
                                ""
                            )
                        |> String.replace "{{FACTOR_CODE}}" factorCode

                newState : Bool -> State
                newState addMinusSign =
                    { state
                        | code = newCode addMinusSign :: state.code
                        , lastTemporaryIndex = state.lastTemporaryIndex + 1
                    }

                isMinus =
                    case maybeSign of
                        Nothing ->
                            False

                        Just Syntax.Expression.Plus ->
                            False

                        Just Syntax.Expression.Minus ->
                            True
            in
            newState isMinus

        ComplexFactor afterFactor ->
            case maybeSign of
                Nothing ->
                    afterFactor

                Just Syntax.Expression.Plus ->
                    afterFactor

                Just Syntax.Expression.Minus ->
                    let
                        newCode =
                            "{{TEMP}} = -1 * {{FACTOR}}"
                                |> String.replace "{{TEMP}}" (temporaryVariable (afterFactor.lastTemporaryIndex + 1))
                                |> String.replace "{{FACTOR}}" (temporaryVariable afterFactor.lastTemporaryIndex)
                    in
                    { afterFactor
                        | code = newCode :: afterFactor.code
                        , lastTemporaryIndex = afterFactor.lastTemporaryIndex + 1
                    }


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
                    let
                        newCode =
                            "{{TEMP}} = {{NAME}}"
                                |> String.replace "{{TEMP}}" (temporaryVariable (state.lastTemporaryIndex + 1))
                                |> String.replace "{{NAME}}" accessors.name
                    in
                    { state
                        | code = newCode :: state.code
                        , lastTemporaryIndex = state.lastTemporaryIndex + 1
                    }
                        |> ComplexFactor

                first :: rest ->
                    let
                        afterFirstExpression : State
                        afterFirstExpression =
                            fromNumericalExpression first state

                        newCodeAfterFirstExpression : String
                        newCodeAfterFirstExpression =
                            "{{TEMP}} = {{NAME}}[{{NUMEXPR}}]"
                                |> String.replace "{{TEMP}}" (temporaryVariable (afterFirstExpression.lastTemporaryIndex + 1))
                                |> String.replace "{{NAME}}" accessors.name
                                |> String.replace "{{NUMEXPR}}" (temporaryVariable afterFirstExpression.lastTemporaryIndex)

                        afterFirst : State
                        afterFirst =
                            { afterFirstExpression
                                | code = newCodeAfterFirstExpression :: afterFirstExpression.code
                                , lastTemporaryIndex = afterFirstExpression.lastTemporaryIndex + 1
                            }

                        afterRest : State
                        afterRest =
                            List.foldl
                                (\currAccessor currState ->
                                    let
                                        afterExpression : State
                                        afterExpression =
                                            fromNumericalExpression currAccessor currState

                                        newCode =
                                            "{{TEMP}} = {{LAST_TEMP}}[{{NUMEXPR}}]"
                                                |> String.replace "{{TEMP}}" (temporaryVariable (afterExpression.lastTemporaryIndex + 1))
                                                |> String.replace "{{LAST_TEMP}}" (temporaryVariable currState.lastTemporaryIndex)
                                                |> String.replace "{{NUMEXPR}}" (temporaryVariable afterExpression.lastTemporaryIndex)
                                    in
                                    { afterExpression
                                        | code = newCode :: afterExpression.code
                                        , lastTemporaryIndex = afterExpression.lastTemporaryIndex + 1
                                    }
                                )
                                afterFirst
                                rest
                    in
                    ComplexFactor afterRest

        Syntax.Expression.ParenthesizedFactor expression ->
            fromNumericalExpression expression state
                |> ComplexFactor

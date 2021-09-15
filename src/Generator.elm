{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Generator exposing (generate, getExpressionValue, initStepperModel)

{-| Generate intermediary code for a `Program`
-}

import Dict exposing (Dict)
import Result.Extra
import Syntax.Expression
import Syntax.Program


{-| Given a program, try to turn it into intermediary code
-}
generate : Syntax.Program.Program -> Result String String
generate program =
    case program of
        Syntax.Program.SingleStatement statement ->
            -- TODO - Implement code generation for single statement
            Err "Implement code generation for single statement"

        Syntax.Program.FunctionList functionList ->
            -- TODO - Implement code generation for function list
            Err "Implement code generation for function list"



-- INTERNAL HELPERS


type alias StepperModel =
    { temporaryValues : List String
    , lastTemporaryIndex : Int
    , symbols : Dict String { type_ : SymbolType }
    , currentCode : String
    }


type SymbolType
    = IntType (Maybe Int)
    | FloatType (Maybe Float)
    | StringType (Maybe String)


initStepperModel : StepperModel
initStepperModel =
    { temporaryValues = []
    , lastTemporaryIndex = -1
    , symbols =
        -- TODO - Remove this after testing
        Dict.fromList
            [ ( "a", { type_ = IntType (Just 5) } )
            , ( "b", { type_ = IntType (Just 3) } )
            , ( "c", { type_ = IntType (Just 8) } )
            ]
    , currentCode = ""
    }


type FactorValue
    = IntFactor Int
    | FloatFactor Float
    | StringFactor String
    | NullFactor


factorValueToString : FactorValue -> String
factorValueToString factorValue =
    case factorValue of
        IntFactor intFactor ->
            String.fromInt intFactor

        FloatFactor floatFactor ->
            String.fromFloat floatFactor

        StringFactor stringFactor ->
            stringFactor

        NullFactor ->
            "null"


getFactorValue : Syntax.Expression.Factor -> StepperModel -> Result String FactorValue
getFactorValue factor stepperModel =
    case factor of
        Syntax.Expression.IntFactor factorValue ->
            Ok (IntFactor factorValue)

        Syntax.Expression.FloatFactor factorValue ->
            Ok (FloatFactor factorValue)

        Syntax.Expression.StringFactor factorValue ->
            Ok (StringFactor factorValue)

        Syntax.Expression.NullFactor ->
            Ok NullFactor

        Syntax.Expression.NamedFactor factorName ->
            let
                nameWithAccessors : Result String String
                nameWithAccessors =
                    factorName.accessors
                        |> List.map (\accessor -> getNumericalExpressionValue accessor stepperModel)
                        |> Result.Extra.combine
                        |> Result.map
                            (List.map factorValueToString
                                >> (\accessors -> factorName.name :: accessors)
                                >> String.join "."
                            )
            in
            case nameWithAccessors of
                Err error ->
                    Err error

                Ok validName ->
                    case Dict.get validName stepperModel.symbols |> Maybe.map .type_ of
                        Nothing ->
                            Err ("I couldn't find `" ++ validName ++ "` on the symbols table. Maybe you need to declare it earlier in the code?")

                        Just (IntType maybeInt) ->
                            maybeInt
                                |> Maybe.map IntFactor
                                |> Maybe.withDefault NullFactor
                                |> Ok

                        Just (FloatType maybeFloat) ->
                            maybeFloat
                                |> Maybe.map FloatFactor
                                |> Maybe.withDefault NullFactor
                                |> Ok

                        Just (StringType maybeString) ->
                            maybeString
                                |> Maybe.map StringFactor
                                |> Maybe.withDefault NullFactor
                                |> Ok

        Syntax.Expression.ParenthesizedFactor factorValue ->
            getNumericalExpressionValue factorValue stepperModel


getUnaryExpressionValue : Syntax.Expression.UnaryExpression -> StepperModel -> Result String FactorValue
getUnaryExpressionValue (Syntax.Expression.UnaryExpression maybeSign factor) stepperModel =
    case getFactorValue factor stepperModel of
        Ok (IntFactor factorValue) ->
            case maybeSign of
                Just Syntax.Expression.Minus ->
                    (-1 * factorValue)
                        |> IntFactor
                        |> Ok

                _ ->
                    Ok (IntFactor factorValue)

        Ok (FloatFactor factorValue) ->
            case maybeSign of
                Just Syntax.Expression.Minus ->
                    (-1 * factorValue)
                        |> FloatFactor
                        |> Ok

                _ ->
                    Ok (FloatFactor factorValue)

        Ok (StringFactor factorValue) ->
            case maybeSign of
                Nothing ->
                    Ok (StringFactor factorValue)

                Just _ ->
                    -- TODO - We need the code's line/col
                    Err "You can't add a sign to a `String`! That doesn't make sense!"

        Ok NullFactor ->
            case maybeSign of
                Nothing ->
                    Ok NullFactor

                Just _ ->
                    Err "Null is a value of its own, and cannot be used in a unary expression!"

        Err error ->
            Err error


getTermValue : Syntax.Expression.Term -> StepperModel -> Result String FactorValue
getTermValue term stepperModel =
    case term of
        Syntax.Expression.SingleTerm unaryExpression ->
            getUnaryExpressionValue unaryExpression stepperModel

        Syntax.Expression.MultipleTerms unaryExpression operator otherTerm ->
            let
                termValue =
                    getTermValue otherTerm stepperModel

                unaryExpressionValue =
                    getUnaryExpressionValue unaryExpression stepperModel
            in
            case ( termValue, unaryExpressionValue ) of
                ( Err error, _ ) ->
                    Err error

                ( _, Err error ) ->
                    Err error

                ( Ok (IntFactor firstValue), Ok (IntFactor secondValue) ) ->
                    case operator of
                        Syntax.Expression.Division ->
                            (toFloat firstValue / toFloat secondValue)
                                |> FloatFactor
                                |> Ok

                        Syntax.Expression.Multiplication ->
                            (firstValue * secondValue)
                                |> IntFactor
                                |> Ok

                        Syntax.Expression.Modulo ->
                            modBy secondValue firstValue
                                |> IntFactor
                                |> Ok

                ( Ok (FloatFactor firstValue), Ok (FloatFactor secondValue) ) ->
                    case operator of
                        Syntax.Expression.Division ->
                            (firstValue / secondValue)
                                |> FloatFactor
                                |> Ok

                        Syntax.Expression.Multiplication ->
                            (firstValue * secondValue)
                                |> FloatFactor
                                |> Ok

                        Syntax.Expression.Modulo ->
                            modBy (floor secondValue) (floor firstValue)
                                |> IntFactor
                                |> Ok

                ( Ok (StringFactor _), _ ) ->
                    Err "I can't apply multiplication, division or modulo to strings!"

                ( _, Ok (StringFactor _) ) ->
                    Err "I can't apply multiplication, division or modulo to strings!"

                _ ->
                    Err "In order to perform multiplication, division or modulo, both the operands must be of the same type (they must be either ints or floats!)"


getNumericalExpressionValue : Syntax.Expression.NumericalExpression -> StepperModel -> Result String FactorValue
getNumericalExpressionValue numericalExpression stepperModel =
    case numericalExpression of
        Syntax.Expression.SingleNumericalExpression term ->
            getTermValue term stepperModel

        Syntax.Expression.MultipleNumericalExpressions term operator otherExpression ->
            let
                termValue =
                    getTermValue term stepperModel

                expressionValue =
                    getNumericalExpressionValue otherExpression stepperModel
            in
            case ( termValue, expressionValue ) of
                ( Err error, _ ) ->
                    Err error

                ( _, Err error ) ->
                    Err error

                ( Ok (IntFactor firstValue), Ok (IntFactor secondValue) ) ->
                    case operator of
                        Syntax.Expression.Addition ->
                            (firstValue + secondValue)
                                |> IntFactor
                                |> Ok

                        Syntax.Expression.Subtraction ->
                            (firstValue - secondValue)
                                |> IntFactor
                                |> Ok

                ( Ok (FloatFactor firstValue), Ok (FloatFactor secondValue) ) ->
                    case operator of
                        Syntax.Expression.Addition ->
                            (firstValue + secondValue)
                                |> FloatFactor
                                |> Ok

                        Syntax.Expression.Subtraction ->
                            (firstValue - secondValue)
                                |> FloatFactor
                                |> Ok

                ( Ok (StringFactor firstValue), Ok (StringFactor secondValue) ) ->
                    case operator of
                        Syntax.Expression.Addition ->
                            (firstValue ++ secondValue)
                                |> StringFactor
                                |> Ok

                        Syntax.Expression.Subtraction ->
                            Err "I can't subtract two `String´s!"

                _ ->
                    Err "In order to perform addition or subtraction, both the operands must be of the same type"


type ExpressionValue
    = IntExpression Int
    | FloatExpression Float
    | StringExpression String
    | BooleanExpression Bool
    | NullExpression


getExpressionValue : Syntax.Expression.Expression -> StepperModel -> Result String ExpressionValue
getExpressionValue expression stepperModel =
    case expression of
        Syntax.Expression.SingleExpression numericalExpression ->
            case getNumericalExpressionValue numericalExpression stepperModel of
                Ok (IntFactor factorValue) ->
                    Ok (IntExpression factorValue)

                Ok (FloatFactor factorValue) ->
                    Ok (FloatExpression factorValue)

                Ok (StringFactor factorValue) ->
                    Ok (StringExpression factorValue)

                Ok NullFactor ->
                    Ok NullExpression

                Err error ->
                    Err error

        Syntax.Expression.WithComparator firstExpression comparator secondExpression ->
            let
                comparatorFunction =
                    case comparator of
                        Syntax.Expression.LessThan ->
                            (<)

                        Syntax.Expression.Greater ->
                            (>)

                        Syntax.Expression.LessThanOrEqualTo ->
                            (<=)

                        Syntax.Expression.GreaterThanOrEqualTo ->
                            (>=)

                        Syntax.Expression.Equals ->
                            (==)

                        Syntax.Expression.Different ->
                            (/=)
            in
            case ( getNumericalExpressionValue firstExpression stepperModel, getNumericalExpressionValue secondExpression stepperModel ) of
                ( Err error, _ ) ->
                    Err error

                ( _, Err error ) ->
                    Err error

                ( Ok (IntFactor firstFactor), Ok (IntFactor secondFactor) ) ->
                    comparatorFunction firstFactor secondFactor
                        |> BooleanExpression
                        |> Ok

                ( Ok (FloatFactor firstFactor), Ok (FloatFactor secondFactor) ) ->
                    comparatorFunction firstFactor secondFactor
                        |> BooleanExpression
                        |> Ok

                ( Ok (StringFactor firstFactor), Ok (StringFactor secondFactor) ) ->
                    comparatorFunction firstFactor secondFactor
                        |> BooleanExpression
                        |> Ok

                ( Ok NullFactor, Ok NullFactor ) ->
                    Ok (BooleanExpression True)

                _ ->
                    Err "In order to compare values, they must be of the same type!"

module Parser.Expression exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Set
import Syntax.Expression as Expression



-- EXPRESSION


parseExpression : String -> Result (List Parser.DeadEnd) String
parseExpression expressionString =
    Parser.run expression expressionString
        |> Result.map Expression.showExpression


expression : Parser Expression.Expression
expression =
    Parser.oneOf
        [ Parser.succeed Expression.WithComparator
            |= numericalExpression
            |. Parser.spaces
            |= comparator
            |. Parser.spaces
            |= numericalExpression
            |> Parser.backtrackable
        , Parser.succeed Expression.SingleExpression
            |= numericalExpression
        ]


comparator : Parser Expression.Comparator
comparator =
    Parser.oneOf
        [ Parser.succeed Expression.LessThanOrEqualTo
            |. Parser.token "<="
        , Parser.succeed Expression.GreatherThanOrEqualTo
            |. Parser.token ">="
        , Parser.succeed Expression.LessThan
            |. Parser.token "<"
        , Parser.succeed Expression.GreatherThan
            |. Parser.token ">"
        , Parser.succeed Expression.Equals
            |. Parser.token "=="
        , Parser.succeed Expression.Different
            |. Parser.token "!="
        ]



-- NUMERICAL EXPRESSION


numericalExpression : Parser Expression.NumericalExpression
numericalExpression =
    Parser.oneOf
        [ Parser.succeed Expression.MultipleNumericalExpressions
            |= term
            |. Parser.spaces
            |= numericalOperator
            |. Parser.spaces
            |= Parser.lazy (\_ -> numericalExpression)
            |> Parser.backtrackable
        , Parser.succeed Expression.SingleNumericalExpression
            |= term
        ]


numericalOperator : Parser Expression.NumericalOperator
numericalOperator =
    Parser.oneOf
        [ Parser.succeed Expression.Addition
            |. Parser.token "+"
        , Parser.succeed Expression.Subtraction
            |. Parser.token "-"
        ]



-- TERM


term : Parser Expression.Term
term =
    Parser.oneOf
        [ Parser.succeed Expression.MultipleTerms
            |= unaryExpression
            |. Parser.spaces
            |= termOperator
            |. Parser.spaces
            |= Parser.lazy (\_ -> term)
            |> Parser.backtrackable
        , Parser.succeed Expression.SingleTerm
            |= unaryExpression
        ]


termOperator : Parser Expression.TermOperator
termOperator =
    Parser.oneOf
        [ Parser.succeed Expression.Multiplication
            |. Parser.token "*"
        , Parser.succeed Expression.Division
            |. Parser.token "/"
        , Parser.succeed Expression.Modulo
            |. Parser.token "%"
        ]



-- UNARY EXPRESSION


unaryExpression : Parser Expression.UnaryExpression
unaryExpression =
    Parser.succeed Expression.UnaryExpression
        |= Parser.oneOf
            [ Parser.succeed (Just Expression.Plus)
                |. Parser.chompIf (\c -> c == '+')
            , Parser.succeed (Just Expression.Minus)
                |. Parser.chompIf (\c -> c == '-')
            , Parser.succeed Nothing
            ]
        |= factor



-- FACTOR


factor : Parser Expression.Factor
factor =
    Parser.oneOf
        [ Parser.number
            { int = Just Expression.IntFactor
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Just Expression.FloatFactor
            }
        , Parser.succeed Expression.StringFactor
            |. Parser.symbol "\""
            |= (Parser.chompUntil "\"" |> Parser.getChompedString)
        , Parser.succeed Expression.NullFactor
            |. Parser.keyword "null"
        , Parser.succeed Expression.NamedFactor
            |= variableAccessor
        , Parser.succeed Expression.ParenthesizedFactor
            |. Parser.token "("
            |. Parser.spaces
            |= Parser.lazy (\_ -> numericalExpression)
            |. Parser.spaces
            |. Parser.token ")"
        ]



-- VARIABLE ACCESSOR


variableName : Parser String
variableName =
    Parser.variable
        { start = Char.isLower
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty -- TODO
        }


variableAccessor : Parser Expression.VariableAccessor
variableAccessor =
    Parser.succeed Expression.VariableAccessor
        |= variableName
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item =
                Parser.succeed identity
                    |. Parser.token "["
                    |= Parser.lazy (\_ -> numericalExpression)
                    |. Parser.token "]"
            , trailing = Parser.Forbidden
            }

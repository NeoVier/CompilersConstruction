module Parser.Expression exposing
    ( expression
    , numericalExpression
    , variableAccessor
    , variableName
    )

import Parser exposing ((|.), (|=), Parser)
import Set
import Syntax.Expression as Expression



-- EXPRESSION


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
                |. Parser.symbol "+"
            , Parser.succeed (Just Expression.Minus)
                |. Parser.symbol "-"
            , Parser.succeed Nothing
            ]
        |= factor



-- FACTOR


string : Parser String
string =
    Parser.succeed ()
        |. Parser.token "\""
        |. Parser.loop '"' stringHelp
        |> Parser.getChompedString
        -- Remove quotes
        |> Parser.map (String.dropLeft 1 >> String.dropRight 1)


stringHelp : Char -> Parser (Parser.Step Char ())
stringHelp separator =
    Parser.oneOf
        [ Parser.succeed (Parser.Done ())
            |. Parser.token (String.fromChar separator)
        , Parser.succeed (Parser.Loop separator)
            |. Parser.chompIf (\char -> char == '\\')
            |. Parser.chompIf (\_ -> True)
        , Parser.succeed (Parser.Loop separator)
            |. Parser.chompIf (\char -> char /= '\\' && char /= separator)
        ]


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
        , string
            |> Parser.map Expression.StringFactor
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
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved =
            Set.fromList
                [ "def"
                , "print"
                , "read"
                , "return"
                , "break"
                , "int"
                , "float"
                , "string"
                , "new"
                , "if"
                , "else"
                , "for"
                ]
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

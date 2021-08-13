{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Parser.Expression exposing
    ( expression
    , numericalExpression
    , variableAccessor
    , variableName
    )

import CCParser exposing (CCParser)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Set
import Syntax.Expression as Expression



-- EXPRESSION


expression : CCParser Expression.Expression
expression =
    Parser.inContext CCParser.Expression <|
        Parser.succeed
            (\numExpr maybeRest ->
                case maybeRest of
                    Nothing ->
                        Expression.SingleExpression numExpr

                    Just ( comp, otherNumExpr ) ->
                        Expression.WithComparator numExpr comp otherNumExpr
            )
            |= numericalExpression
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed (\comp numExpr -> Just ( comp, numExpr ))
                    |= comparator
                    |. Parser.spaces
                    |= numericalExpression
                , Parser.succeed Nothing
                ]


comparator : CCParser Expression.Comparator
comparator =
    Parser.oneOf
        [ Parser.succeed Expression.LessThanOrEqualTo
            |. Parser.token (Parser.Token "<=" CCParser.ExpectingComparator)
        , Parser.succeed Expression.GreatherThanOrEqualTo
            |. Parser.token (Parser.Token ">=" CCParser.ExpectingComparator)
        , Parser.succeed Expression.LessThan
            |. Parser.token (Parser.Token "<" CCParser.ExpectingComparator)
        , Parser.succeed Expression.GreatherThan
            |. Parser.token (Parser.Token ">" CCParser.ExpectingComparator)
        , Parser.succeed Expression.Equals
            |. Parser.token (Parser.Token "==" CCParser.ExpectingComparator)
        , Parser.succeed Expression.Different
            |. Parser.token (Parser.Token "!=" CCParser.ExpectingComparator)
        ]



-- NUMERICAL EXPRESSION


numericalExpression : CCParser Expression.NumericalExpression
numericalExpression =
    Parser.inContext CCParser.NumericalExpression <|
        Parser.succeed
            (\term_ maybeRest ->
                case maybeRest of
                    Nothing ->
                        Expression.SingleNumericalExpression term_

                    Just ( operator, numExpr ) ->
                        Expression.MultipleNumericalExpressions term_ operator numExpr
            )
            |= term
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed (\operator numExpr -> Just ( operator, numExpr ))
                    |= numericalOperator
                    |. Parser.spaces
                    |= Parser.lazy (\_ -> numericalExpression)
                , Parser.succeed Nothing
                ]


numericalOperator : CCParser Expression.NumericalOperator
numericalOperator =
    Parser.oneOf
        [ Parser.succeed Expression.Addition
            |. Parser.token (Parser.Token "+" CCParser.ExpectingNumericalOperator)
        , Parser.succeed Expression.Subtraction
            |. Parser.token (Parser.Token "-" CCParser.ExpectingNumericalOperator)
        ]



-- TERM


term : CCParser Expression.Term
term =
    Parser.oneOf
        [ Parser.inContext CCParser.MultipleTerms <|
            (Parser.succeed Expression.MultipleTerms
                |= unaryExpression
                |. Parser.spaces
                |= termOperator
                |. Parser.spaces
                |= Parser.lazy (\_ -> term)
                |> Parser.backtrackable
            )
        , Parser.inContext CCParser.SingleTerm <|
            Parser.succeed Expression.SingleTerm
                |= unaryExpression
        ]


termOperator : CCParser Expression.TermOperator
termOperator =
    Parser.oneOf
        [ Parser.succeed Expression.Multiplication
            |. Parser.token (Parser.Token "*" CCParser.ExpectingTermOperator)
        , Parser.succeed Expression.Division
            |. Parser.token (Parser.Token "/" CCParser.ExpectingTermOperator)
        , Parser.succeed Expression.Modulo
            |. Parser.token (Parser.Token "%" CCParser.ExpectingTermOperator)
        ]



-- UNARY EXPRESSION


unaryExpression : CCParser Expression.UnaryExpression
unaryExpression =
    Parser.inContext CCParser.UnaryExpression <|
        Parser.oneOf
            [ Parser.succeed (Expression.UnaryExpression (Just Expression.Plus))
                |. Parser.symbol (Parser.Token "+" CCParser.ExpectingSign)
                |= factor
            , Parser.succeed (Expression.UnaryExpression (Just Expression.Minus))
                |. Parser.symbol (Parser.Token "-" CCParser.ExpectingSign)
                |= factor
            , factor
                |> Parser.map (Expression.UnaryExpression Nothing)
            ]



-- FACTOR


string : Maybe CCParser.Problem -> CCParser String
string maybeProblem =
    Parser.inContext CCParser.ParsingString <|
        (Parser.succeed ()
            |. Parser.token (Parser.Token "\"" (Maybe.withDefault CCParser.ExpectingStartOfString maybeProblem))
            |. Parser.loop '"' stringHelp
            |> Parser.getChompedString
            -- Remove quotes
            |> Parser.map (String.dropLeft 1 >> String.dropRight 1)
        )


stringHelp : Char -> CCParser (Parser.Step Char ())
stringHelp separator =
    Parser.oneOf
        [ Parser.succeed (Parser.Done ())
            |. Parser.token (Parser.Token (String.fromChar separator) (CCParser.ExpectingCharacter separator))
        , Parser.succeed (Parser.Loop separator)
            |. Parser.chompIf (\char -> char == '\\') CCParser.ExpectingBackSlash
            |. Parser.chompIf (\_ -> True) CCParser.ExpectingAnything
        , Parser.succeed (Parser.Loop separator)
            |. Parser.chompIf (\char -> char /= '\\' && char /= separator)
                (CCParser.ExpectingDifferentThan [ '\\', separator ])
        ]


factor : CCParser Expression.Factor
factor =
    Parser.inContext CCParser.Factor <|
        Parser.oneOf
            [ Parser.number
                { int = Ok Expression.IntFactor
                , hex = Err CCParser.ExpectingBase10
                , octal = Err CCParser.ExpectingBase10
                , binary = Err CCParser.ExpectingBase10
                , float = Ok Expression.FloatFactor
                , invalid = CCParser.ExpectingNumber
                , expecting = CCParser.ExpectingFactor
                }
                |> Parser.backtrackable
            , string (Just CCParser.ExpectingFactor)
                |> Parser.map Expression.StringFactor
            , Parser.succeed Expression.NullFactor
                |. Parser.keyword (Parser.Token "null" CCParser.ExpectingFactor)
            , Parser.succeed Expression.NamedFactor
                |= variableAccessor
            , Parser.succeed Expression.ParenthesizedFactor
                |. Parser.token (Parser.Token "(" CCParser.ExpectingFactor)
                |. Parser.spaces
                |= Parser.lazy (\_ -> numericalExpression)
                |. Parser.spaces
                |. Parser.token (Parser.Token ")" CCParser.ExpectingCloseParens)
            ]



-- VARIABLE ACCESSOR


variableName : CCParser String
variableName =
    Parser.inContext CCParser.VariableName <|
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
            , expecting = CCParser.ExpectingVariableName
            }


variableAccessor : CCParser Expression.VariableAccessor
variableAccessor =
    Parser.succeed Expression.VariableAccessor
        |= variableName
        |= Parser.sequence
            { start = Parser.Token "" CCParser.ExpectingNoStart
            , separator = Parser.Token "" CCParser.ExpectingNoSeparator
            , end = Parser.Token "" CCParser.ExpectingNoEnd
            , spaces = Parser.spaces
            , item =
                Parser.succeed identity
                    |. Parser.token (Parser.Token "[" CCParser.ExpectingOpenBracket)
                    |= Parser.lazy (\_ -> numericalExpression)
                    |. Parser.token (Parser.Token "]" CCParser.ExpectingCloseBracket)
            , trailing = Parser.Forbidden
            }

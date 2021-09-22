{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Parser.Statement exposing (functionDeclaration, statement)

import CCParser exposing (CCParser)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Expression as Expression
import Syntax.Statement as Statement



-- STATEMENT


statement : CCParser Statement.Statement
statement =
    Parser.inContext CCParser.Statement <|
        Parser.oneOf
            [ Parser.succeed
                (\start declaration_ end ->
                    Statement.VariableDeclaration declaration_ { start = start, end = end }
                )
                |= Parser.getPosition
                |= declaration
                |. Parser.spaces
                |. Parser.symbol (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
                |= Parser.getPosition
            , Parser.succeed Statement.AttributionStatement
                |= attribution
                |. Parser.spaces
                |. Parser.symbol (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
            , Parser.inContext CCParser.PrintStatement <|
                Parser.succeed Statement.PrintStatement
                    |. Parser.keyword (Parser.Token "print" CCParser.ExpectingStatement)
                    |. Parser.spaces
                    |= Expression.expression
                    |. Parser.spaces
                    |. Parser.symbol (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
            , Parser.inContext CCParser.ReadStatement <|
                Parser.succeed Statement.ReadStatement
                    |. Parser.keyword (Parser.Token "read" CCParser.ExpectingStatement)
                    |. Parser.spaces
                    |= Expression.variableAccessor
                    |. Parser.spaces
                    |. Parser.symbol (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
                    |. Parser.spaces
            , Parser.inContext CCParser.ReturnStatement <|
                Parser.succeed
                    (\start end ->
                        Statement.ReturnStatement { start = start, end = end }
                    )
                    |= Parser.getPosition
                    |. Parser.keyword (Parser.Token "return" CCParser.ExpectingStatement)
                    |. Parser.spaces
                    |. Parser.symbol (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
                    |= Parser.getPosition
            , ifStatement
                |> Parser.map Statement.IfStatement
            , forStatement
                |> Parser.map Statement.ForStatement
            , statementList
                |> Parser.map Statement.StatementBlock
            , Parser.inContext CCParser.BreakStatement <|
                Parser.succeed (\start end -> Statement.BreakStatement { start = start, end = end })
                    |= Parser.getPosition
                    |. Parser.keyword (Parser.Token "break" CCParser.ExpectingStatement)
                    |. Parser.symbol (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
                    |= Parser.getPosition
            , Parser.succeed Statement.Semicolon
                |. Parser.symbol (Parser.Token ";" CCParser.ExpectingStatement)
            ]



-- VARIABLE DECLARATION


declaration : CCParser Statement.Declaration
declaration =
    Parser.succeed Statement.Declaration
        |= variableType
        |. Parser.spaces
        |= Expression.variableName
        |. Parser.spaces
        |= Parser.sequence
            { start = Parser.Token "" CCParser.ExpectingNoStart
            , separator = Parser.Token "" CCParser.ExpectingNoSeparator
            , end = Parser.Token "" CCParser.ExpectingNoEnd
            , spaces = Parser.spaces
            , item =
                Parser.succeed identity
                    |. Parser.token (Parser.Token "[" CCParser.ExpectingOpenBracket)
                    |= Parser.int CCParser.ExpectingInt CCParser.ExpectingBase10Int
                    |. Parser.token (Parser.Token "]" CCParser.ExpectingCloseBracket)
            , trailing = Parser.Forbidden
            }
        |> Parser.inContext CCParser.VariableDeclaration


variableType : CCParser Statement.VariableType
variableType =
    Parser.inContext CCParser.VariableType <|
        Parser.oneOf
            [ Parser.succeed Statement.IntVariable
                |. Parser.keyword (Parser.Token "int" CCParser.ExpectingVariableType)
            , Parser.succeed Statement.FloatVariable
                |. Parser.keyword (Parser.Token "float" CCParser.ExpectingVariableType)
            , Parser.succeed Statement.StringVariable
                |. Parser.keyword (Parser.Token "string" CCParser.ExpectingVariableType)
            ]



-- ATTRIBUTION STATEMENT


attribution : CCParser Statement.Attribution
attribution =
    Parser.inContext CCParser.AttributionStatement <|
        Parser.succeed
            (\start accessor value end ->
                Statement.Attribution accessor value { start = start, end = end }
            )
            |= Parser.getPosition
            |= Expression.variableAccessor
            |. Parser.spaces
            |. Parser.token (Parser.Token "=" (CCParser.ExpectingCharacter '='))
            |. Parser.spaces
            |= Parser.oneOf
                [ functionCall
                    |> Parser.map Statement.FunctionCallAttribution
                    |> Parser.backtrackable
                , Expression.expression
                    |> Parser.map Statement.ExpressionAttribution
                    |> Parser.backtrackable
                , allocation
                    |> Parser.map Statement.AllocationExpression
                ]
            |= Parser.getPosition


allocation : CCParser Statement.Allocation
allocation =
    let
        accessorParser =
            Parser.succeed identity
                |. Parser.token (Parser.Token "[" CCParser.ExpectingOpenBracket)
                |= Expression.numericalExpression
                |. Parser.token (Parser.Token "]" CCParser.ExpectingCloseBracket)
    in
    Parser.inContext CCParser.Allocation <|
        Parser.succeed Statement.Allocation
            |. Parser.keyword (Parser.Token "new" (CCParser.ExpectingKeyword "new"))
            |. Parser.spaces
            |= variableType
            |. Parser.spaces
            |= accessorParser
            |. Parser.spaces
            |= Parser.sequence
                { start = Parser.Token "" CCParser.ExpectingNoStart
                , separator = Parser.Token "" CCParser.ExpectingNoSeparator
                , end = Parser.Token "" CCParser.ExpectingNoEnd
                , spaces = Parser.spaces
                , item = accessorParser
                , trailing = Parser.Forbidden
                }


functionCall : CCParser Statement.FunctionCall
functionCall =
    Parser.inContext CCParser.FunctionCall <|
        Parser.succeed Statement.FunctionCall
            |= Expression.variableName
            |. Parser.spaces
            |= Parser.sequence
                { start = Parser.Token "(" CCParser.ExpectingOpenParens
                , separator = Parser.Token "," CCParser.ExpectingComma
                , end = Parser.Token ")" CCParser.ExpectingCloseParens
                , spaces = Parser.spaces
                , item = Expression.variableName
                , trailing = Parser.Optional
                }



-- IF STATEMENT


ifStatement : CCParser Statement.If
ifStatement =
    Parser.inContext CCParser.IfStatement <|
        Parser.succeed Statement.If
            |. Parser.keyword (Parser.Token "if" CCParser.ExpectingIf)
            |. Parser.spaces
            |. Parser.token (Parser.Token "(" CCParser.ExpectingOpenParens)
            |. Parser.spaces
            |= Expression.expression
            |. Parser.spaces
            |. Parser.token (Parser.Token ")" CCParser.ExpectingCloseParens)
            |. Parser.spaces
            |= Parser.lazy (\_ -> statement)
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed Just
                    |. Parser.keyword (Parser.Token "else" (CCParser.ExpectingKeyword "else"))
                    |. Parser.spaces
                    |= Parser.lazy (\_ -> statement)
                , Parser.succeed Nothing
                ]



-- FOR STATEMENT


forStatement : CCParser Statement.For
forStatement =
    Parser.inContext CCParser.ForStatement <|
        Parser.succeed Statement.For
            |. Parser.keyword (Parser.Token "for" CCParser.ExpectingFor)
            |. Parser.spaces
            |. Parser.token (Parser.Token "(" CCParser.ExpectingOpenParens)
            |. Parser.spaces
            |= attribution
            |. Parser.spaces
            |. Parser.token (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
            |. Parser.spaces
            |= Expression.expression
            |. Parser.spaces
            |. Parser.token (Parser.Token ";" (CCParser.ExpectingCharacter ';'))
            |. Parser.spaces
            |= attribution
            |. Parser.spaces
            |. Parser.token (Parser.Token ")" CCParser.ExpectingCloseParens)
            |. Parser.spaces
            |= Parser.lazy (\_ -> statement)


statementList : CCParser Statement.StatementList
statementList =
    Parser.inContext CCParser.StatementList <|
        Parser.succeed Statement.StatementList
            |. Parser.token (Parser.Token "{" CCParser.ExpectingStatementList)
            |. Parser.spaces
            |= Parser.lazy (\_ -> statement)
            |. Parser.spaces
            |= Parser.sequence
                { start = Parser.Token "" CCParser.ExpectingNoStart
                , separator = Parser.Token "" CCParser.ExpectingNoSeparator
                , end = Parser.Token "" CCParser.ExpectingNoEnd
                , spaces = Parser.spaces
                , item = Parser.lazy (\_ -> statement)
                , trailing = Parser.Forbidden
                }
            |. Parser.token (Parser.Token "}" CCParser.ExpectingCloseCurlies)



-- FUNCTION DECLARATION


functionDeclaration : CCParser Statement.FunctionDeclaration
functionDeclaration =
    Parser.inContext CCParser.FunctionDeclaration <|
        Parser.succeed Statement.FunctionDeclaration
            |. Parser.keyword (Parser.Token "def" (CCParser.ExpectingKeyword "def"))
            |. Parser.spaces
            |= Expression.variableName
            |. Parser.spaces
            |= Parser.sequence
                { start = Parser.Token "(" CCParser.ExpectingOpenParens
                , separator = Parser.Token "," CCParser.ExpectingComma
                , end = Parser.Token ")" CCParser.ExpectingCloseParens
                , spaces = Parser.spaces
                , item = functionParameter
                , trailing = Parser.Optional
                }
            |. Parser.spaces
            |= statementList


functionParameter : CCParser Statement.FunctionParameter
functionParameter =
    Parser.succeed Statement.FunctionParameter
        |= variableType
        |. Parser.spaces
        |= Expression.variableName

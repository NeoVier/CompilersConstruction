module Parser.Statement exposing (..)

import Parser exposing ((|.), (|=), Parser)
import Parser.Expression as Expression
import Syntax.Statement as Statement



-- STATEMENT


statement : Parser Statement.Statement
statement =
    Parser.oneOf
        [ Parser.succeed Statement.VariableDeclaration
            |= declaration
            |. Parser.spaces
            |. Parser.token ";"
        , Parser.succeed Statement.AttributionStatement
            |= attribution
            |. Parser.spaces
            |. Parser.token ";"
        , Parser.succeed Statement.PrintStatement
            |. Parser.keyword "print"
            |. Parser.spaces
            |= Expression.expression
            |. Parser.spaces
            |. Parser.token ";"
        , Parser.succeed Statement.ReadStatement
            |. Parser.keyword "read"
            |. Parser.spaces
            |= Expression.variableAccessor
            |. Parser.spaces
            |. Parser.token ";"
            |. Parser.spaces
        , Parser.succeed Statement.ReturnStatement
            |. Parser.keyword "return"
            |. Parser.spaces
            |. Parser.token ";"
        , ifStatement
            |> Parser.map Statement.IfStatement
        , forStatement
            |> Parser.map Statement.ForStatement
        , statementList
            |> Parser.map Statement.StatementBlock
        , Parser.succeed Statement.BreakStatement
            |. Parser.keyword "break"
            |. Parser.token ";"
        , Parser.succeed Statement.Semicolon
            |. Parser.token ";"
        ]



-- VARIABLE DECLARATION


declaration : Parser Statement.Declaration
declaration =
    Parser.succeed Statement.Declaration
        |= variableType
        |. Parser.spaces
        |= Expression.variableName
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item =
                Parser.succeed identity
                    |. Parser.token "["
                    |= Parser.int
                    |. Parser.token "]"
            , trailing = Parser.Forbidden
            }


variableType : Parser Statement.VariableType
variableType =
    Parser.oneOf
        [ Parser.succeed Statement.IntVariable
            |. Parser.keyword "int"
        , Parser.succeed Statement.FloatVariable
            |. Parser.keyword "float"
        , Parser.succeed Statement.StringVariable
            |. Parser.keyword "string"
        ]



-- ATTRIBUTION STATEMENT


attribution : Parser Statement.Attribution
attribution =
    Parser.succeed Statement.Attribution
        |= Expression.variableAccessor
        |. Parser.spaces
        |= Parser.oneOf
            [ Expression.expression
                |> Parser.map Statement.ExpressionAttribution
            , allocation
                |> Parser.map Statement.AllocationExpression
            , functionCall
                |> Parser.map Statement.FunctionCallAttribution
            ]


allocation : Parser Statement.Allocation
allocation =
    let
        accessorParser =
            Parser.succeed identity
                |. Parser.token "["
                |= Expression.numericalExpression
                |. Parser.token "]"
    in
    Parser.succeed Statement.Allocation
        |. Parser.keyword "new"
        |. Parser.spaces
        |= variableType
        |. Parser.spaces
        |= accessorParser
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = accessorParser
            , trailing = Parser.Forbidden
            }


functionCall : Parser Statement.FunctionCall
functionCall =
    Parser.succeed Statement.FunctionCall
        |= Expression.variableName
        |. Parser.spaces
        |= Parser.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = Parser.spaces
            , item = Expression.variableName
            , trailing = Parser.Optional
            }



-- IF STATEMENT


ifStatement : Parser Statement.If
ifStatement =
    Parser.succeed Statement.If
        |. Parser.keyword "if"
        |. Parser.spaces
        |. Parser.token "("
        |. Parser.spaces
        |= Expression.expression
        |. Parser.spaces
        |. Parser.token ")"
        |. Parser.spaces
        |= Parser.lazy (\_ -> statement)
        |= Parser.oneOf
            [ Parser.succeed Just
                |. Parser.spaces
                |. Parser.keyword "else"
                |. Parser.spaces
                |= Parser.lazy (\_ -> statement)
            , Parser.succeed Nothing
            ]



-- FOR STATEMENT


forStatement : Parser Statement.For
forStatement =
    Parser.succeed Statement.For
        |. Parser.keyword "for"
        |. Parser.spaces
        |. Parser.token "("
        |. Parser.spaces
        |= attribution
        |. Parser.spaces
        |. Parser.token ";"
        |. Parser.spaces
        |= Expression.expression
        |. Parser.spaces
        |. Parser.token ";"
        |. Parser.spaces
        |= attribution
        |. Parser.spaces
        |= Parser.lazy (\_ -> statement)


statementList : Parser Statement.StatementList
statementList =
    Parser.succeed Statement.StatementList
        |. Parser.token "{"
        |. Parser.spaces
        |= Parser.lazy (\_ -> statement)
        |. Parser.spaces
        |= Parser.sequence
            { start = ""
            , separator = ""
            , end = ""
            , spaces = Parser.spaces
            , item = Parser.lazy (\_ -> statement)
            , trailing = Parser.Forbidden
            }
        |. Parser.token "}"

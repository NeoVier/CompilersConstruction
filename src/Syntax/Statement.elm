{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Syntax.Statement exposing
    ( Allocation
    , Attribution
    , AttributionValue(..)
    , Declaration
    , For
    , FunctionCall
    , FunctionDeclaration
    , FunctionParameter
    , If
    , ParameterList
    , Statement(..)
    , StatementList
    , VariableType(..)
    , show
    , showFunctionDeclaration
    , showVariableType
    )

{-| Statements are what let us do complex branching, repeat code,
declare functions, and more. They mostly build on top of `Expressions`, so we
can have complex code, built on top of simple code
-}

import CCParser
import Syntax.Expression as Expression



-- STATEMENT


{-| There are a bunch of possible kinds of `Statement`s. This is the definition
of all of them!
-}
type Statement
    = VariableDeclaration Declaration CCParser.Range
    | AttributionStatement Attribution
    | PrintStatement Expression.Expression
    | ReadStatement Expression.VariableAccessor
    | ReturnStatement CCParser.Range
    | IfStatement If
    | ForStatement For
    | StatementBlock StatementList
    | BreakStatement CCParser.Range
    | Semicolon



-- VARIABLE DECLARATION


{-| Our language supports these kinds of variables
-}
type VariableType
    = IntVariable
    | FloatVariable
    | StringVariable


{-| A `Declaration` refers to declaring a variable. For that, we need a
[`VariableType`](#VariableType), a name, and some dimmensions, in case the
variable is actually an array
-}
type alias Declaration =
    { type_ : VariableType
    , name : String
    , dimmensions : List Int
    }



-- ATTRIBUTION STATEMENT


{-| This is what we can attribute to a variable. We can call a function, call an
expression, or allocate some memory
-}
type AttributionValue
    = ExpressionAttribution Expression.Expression
    | AllocationExpression Allocation
    | FunctionCallAttribution FunctionCall


{-| An `Attribution` needs a [`VariableAccessor`](#VariableAccessor) to
attribute to, and an [`AttributionValue`](#AttributionValue) to put on that
variable
-}
type alias Attribution =
    { variableAccessor : Expression.VariableAccessor
    , value : AttributionValue
    , range : CCParser.Range
    }


{-| To allocate memory, we need to know the [`VariableType`](#VariableType) and
a non-empty list of dimmensions
-}
type alias Allocation =
    { type_ : VariableType
    , firstDimmension : Expression.NumericalExpression
    , dimmensions : List Expression.NumericalExpression
    }


{-| To call a function, we need to know the function's name, and we need the
[`ParameterList`](#ParameterList) to pass to that function
-}
type alias FunctionCall =
    { functionName : String
    , parameters : ParameterList
    , range : CCParser.Range
    }


{-| A parameter list is just a list of variable names
-}
type alias ParameterList =
    List String



-- IF STATEMENT


{-| An if statement needs a condition, and a body. Additionally, it might have
an else body
-}
type alias If =
    { condition : Expression.Expression
    , body : Statement
    , elseBody : Maybe Statement
    }



-- FOR STATEMENT


{-| We have C-style for statements, so we need an [`Attribution`](#Attribution),
a condition, and an increment statement. We also need the body of the for loop
-}
type alias For =
    { attribution : Attribution
    , condition : Expression.Expression
    , increment : Attribution
    , body : Statement
    }



-- STATEMENT LIST


{-| A statement list is just a non-empty list of [`Statement`s](#Statement)
-}
type alias StatementList =
    { firstStatement : Statement
    , otherStatements : List Statement
    }



-- FUNCTION DECLARATION


{-| To declare a function, we need it's name and
[`Parameter`s](#FunctionParameter) for it's signature, and we also need a body
-}
type alias FunctionDeclaration =
    { name : String
    , parameters : List FunctionParameter
    , definitionRange : CCParser.Range
    , body : StatementList
    }


{-| A `FunctionParameter` is just a variable name, with a static
[`VariableType`](#VariableType)
-}
type alias FunctionParameter =
    { type_ : VariableType
    , name : String
    , definitionRange : CCParser.Range
    }



-- SHOW


{-| Turn a `Statement` into a `String`
-}
show : Statement -> String
show statement =
    case statement of
        VariableDeclaration declaration _ ->
            showDeclaration declaration ++ ";"

        AttributionStatement attribution ->
            showAttribution attribution ++ ";"

        PrintStatement expression ->
            showPrintStatement expression ++ ";"

        ReadStatement variableAccessor ->
            showReadStatement variableAccessor ++ ";"

        ReturnStatement _ ->
            "return;"

        IfStatement if_ ->
            showIfStatement if_

        ForStatement for ->
            showForStatement for

        StatementBlock statementList ->
            showStatementList statementList

        BreakStatement _ ->
            "break;"

        Semicolon ->
            ";"


{-| Turn a `FunctionDeclaration` into a `String`
-}
showFunctionDeclaration : FunctionDeclaration -> String
showFunctionDeclaration functionDeclaration =
    "def "
        ++ functionDeclaration.name
        ++ "("
        ++ (functionDeclaration.parameters
                |> List.map showFunctionParameter
                |> String.join ", "
           )
        ++ ")\n"
        ++ showStatementList functionDeclaration.body


{-| Turn a `VariableType` into a `String`
-}
showVariableType : VariableType -> String
showVariableType variableType =
    case variableType of
        IntVariable ->
            "int"

        FloatVariable ->
            "float"

        StringVariable ->
            "string"



-- INTERNAL SHOW HELPERS


showDeclaration : Declaration -> String
showDeclaration declaration =
    showVariableType declaration.type_
        ++ " "
        ++ declaration.name
        ++ (declaration.dimmensions
                |> List.map (\dimmension -> "[" ++ String.fromInt dimmension ++ "]")
                |> String.concat
           )


showAttribution : Attribution -> String
showAttribution attribution =
    Expression.showVariableAccessor attribution.variableAccessor
        ++ " = "
        ++ showAttributionValue attribution.value


showPrintStatement : Expression.Expression -> String
showPrintStatement expression =
    "print " ++ Expression.show expression


showReadStatement : Expression.VariableAccessor -> String
showReadStatement variableAccessor =
    "read " ++ Expression.showVariableAccessor variableAccessor


showIfStatement : If -> String
showIfStatement if_ =
    let
        showElse =
            case if_.elseBody of
                Nothing ->
                    ""

                Just elseBody ->
                    "\nelse\n" ++ show elseBody
    in
    "if ("
        ++ Expression.show if_.condition
        ++ ")\n"
        ++ show if_.body
        ++ showElse


showForStatement : For -> String
showForStatement for =
    "for ("
        ++ showAttribution for.attribution
        ++ "; "
        ++ Expression.show for.condition
        ++ "; "
        ++ showAttribution for.increment
        ++ ")\n"
        ++ show for.body


showStatementList : StatementList -> String
showStatementList statementList =
    "{\n"
        ++ ((statementList.firstStatement :: statementList.otherStatements)
                |> List.map show
                |> String.join "\n"
           )
        ++ "\n}"


showAttributionValue : AttributionValue -> String
showAttributionValue attributionValue =
    case attributionValue of
        ExpressionAttribution expression ->
            Expression.show expression

        AllocationExpression allocation ->
            showAllocation allocation

        FunctionCallAttribution functionCall ->
            showFunctionCall functionCall


showAllocation : Allocation -> String
showAllocation allocation =
    "new "
        ++ showVariableType allocation.type_
        ++ ((allocation.firstDimmension :: allocation.dimmensions)
                |> List.map (\dimmension -> "[" ++ Expression.showNumericalExpression dimmension ++ "]")
                |> String.concat
           )


showFunctionCall : FunctionCall -> String
showFunctionCall functionCall =
    functionCall.functionName ++ "(" ++ String.join ", " functionCall.parameters ++ ")"


showFunctionParameter : FunctionParameter -> String
showFunctionParameter functionParameter =
    [ showVariableType functionParameter.type_
    , functionParameter.name
    ]
        |> String.join " "

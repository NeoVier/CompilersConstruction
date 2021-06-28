module Syntax.Statement exposing
    ( Allocation
    , Attribution
    , AttributionValue(..)
    , Declaration
    , For
    , FunctionCall
    , If
    , ParameterList
    , Statement(..)
    , StatementList
    , VariableType(..)
    )

import Syntax.Expression as Expression



-- STATEMENT


type Statement
    = VariableDeclaration Declaration
    | AttributionStatement Attribution
    | PrintStatement Expression.Expression
    | ReadStatement Expression.VariableAccessor
    | ReturnStatement
    | IfStatement If
    | ForStatement For
    | StatementBlock StatementList
    | BreakStatement
    | Semicolon



-- VARIABLE DECLARATION


type VariableType
    = IntVariable
    | FloatVariable
    | StringVariable


type alias Declaration =
    { type_ : VariableType
    , name : String
    , dimmensions : List Int
    }



-- ATTRIBUTION STATEMENT


type AttributionValue
    = ExpressionAttribution Expression.Expression
    | AllocationExpression Allocation
    | FunctionCallAttribution FunctionCall


type alias Attribution =
    { variableAccessor : Expression.VariableAccessor
    , value : AttributionValue
    }


type alias Allocation =
    { type_ : VariableType
    , firstDimmension : Expression.NumericalExpression
    , dimmensions : List Expression.NumericalExpression
    }


type alias FunctionCall =
    { functionName : String
    , parameters : ParameterList
    }


type alias ParameterList =
    List String



-- IF STATEMENT


type alias If =
    { condition : Expression.Expression
    , body : Statement
    , elseBody : Maybe Statement
    }



-- FOR STATEMENT


type alias For =
    { attribution : Attribution
    , condition : Expression.Expression
    , increment : Attribution
    , body : Statement
    }



-- STATEMENT LIST


type alias StatementList =
    { firstStatement : Statement
    , otherStatements : List Statement
    }

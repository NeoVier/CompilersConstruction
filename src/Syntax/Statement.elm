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
    )

{-| Statements are what let us do complex branching, repeat code,
declare functions, and more. They mostly build on top of `Expressions`, so we
can have complex code, built on top of simple code
-}

import Syntax.Expression as Expression



-- STATEMENT


{-| There are a bunch of possible kinds of `Statement`s. This is the definition
of all of them!
-}
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
    , body : StatementList
    }


{-| A `FunctionParameter` is just a variable name, with a static
[`VariableType`](#VariableType)
-}
type alias FunctionParameter =
    { type_ : VariableType
    , name : String
    }

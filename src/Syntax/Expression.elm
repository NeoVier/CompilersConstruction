{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Syntax.Expression exposing
    ( Comparator(..)
    , Expression(..)
    , Factor(..)
    , NumericalExpression(..)
    , NumericalOperator(..)
    , Sign(..)
    , Term(..)
    , TermOperator(..)
    , UnaryExpression(..)
    , VariableAccessor
    , show
    , showNumericalExpression
    , showVariableAccessor
    )

{-| Expressions are essential to the language. They describe how we can perform
mathematical operations, compare expressions with each other, and reference
variables
-}

-- EXPRESSION


{-| Represents a valid `Expression`. From the grammar provided, an expression
can either be a single [`NumericalExpression`](#NumericalExpression), or two
`NumericalExpression`s, joined together by a [`Comparator`](#Comparator)
function
-}
type Expression
    = SingleExpression NumericalExpression
    | WithComparator NumericalExpression Comparator NumericalExpression


{-| Represents all the possible boolean `Comparator`s, such as `>` and `>`
-}
type Comparator
    = LessThan
    | GreatherThan
    | LessThanOrEqualTo
    | GreatherThanOrEqualTo
    | Equals
    | Different



-- NUMERICAL EXPRESSION


{-| From the grammar, a `NumericalExpression` can either be a single
[`Term`](#Term), or multiple `Terms` joined together by a
[`NumericalOperator`](#NumericalOperator) (`Addition` or `Subtraction`). This is
achieved here by forming a tree-like structure, where each element can either be
a leaf of the tree, or a node that has a `Term`, a `NumericalOperator` and a
single `NumericalExpression` child:

        Term +
            (Term -
                (Term +
                    (Term)
                )
            )

-}
type NumericalExpression
    = SingleNumericalExpression Term
    | MultipleNumericalExpressions Term NumericalOperator NumericalExpression


{-| A `NumericalOperator` describes the lowest-priority operators, such as
`Addition` and `Subtraction`
-}
type NumericalOperator
    = Addition
    | Subtraction



-- TERM


{-| A `Term` joins [`UnaryExpression`s](#UnaryExpression) with higher-priority
operators, such as `Multiplication` and `Division`. It follows a very similar
structure to [`NumericalExpression`](#NumericalExpression)
-}
type Term
    = SingleTerm UnaryExpression
    | MultipleTerms UnaryExpression TermOperator Term


{-| A `TermOperator` describes the highest-priority binary operators, such as
`Multiplication` and `Division`
-}
type TermOperator
    = Multiplication
    | Division
    | Modulo



-- UNARY EXPRESSION


{-| A `UnaryExpression` is used to add (optional) unary operators
(such as [`Sign`](#Sign)) to a [`Factor`](#Factor)
-}
type UnaryExpression
    = UnaryExpression (Maybe Sign) Factor


{-| This defines which kind of signs we can have in a
[`UnaryExpression`](#UnaryExpression)
-}
type Sign
    = Plus
    | Minus



-- FACTOR


{-| The `Factor` is the lowest-level token, such as ints, floats, strings and
named variables. It also provides the ability to have
[`NumericalExpression`s](#NumericalExpression) prioritized with parenthesis
-}
type Factor
    = IntFactor Int
    | FloatFactor Float
    | StringFactor String
    | NullFactor
    | NamedFactor VariableAccessor
    | ParenthesizedFactor NumericalExpression



-- VARIABLE ACCESSOR


{-| We use `VariableAccessor`s to access the indexes of a variable:

```c
    x = new int[10][10];
    x[5][10]; // This is a VariableAccessor: `{ name = x, accessors = [5, 10] }`

    x; // This is a VariableAccessor: `{ name = x, accessors = [] }`
```

-}
type alias VariableAccessor =
    { name : String, accessors : List NumericalExpression }



-- SHOW


{-| This is useful for debugging purposes. It turns an
[`Expression`](#Expression) into a more human-readable format. It's pretty much
the reverse of parsing the expression.
-}
show : Expression -> String
show expression =
    case expression of
        SingleExpression numericalExpression ->
            showNumericalExpression numericalExpression

        WithComparator firstExpression comparator secondExpression ->
            String.join " "
                [ showNumericalExpression firstExpression
                , showComparator comparator
                , showNumericalExpression secondExpression
                ]


{-| Turn a `NumericalExpression` into a `String`
-}
showNumericalExpression : NumericalExpression -> String
showNumericalExpression numericalExpression =
    case numericalExpression of
        SingleNumericalExpression term ->
            showTerm term

        MultipleNumericalExpressions firstTerm operator otherExpression ->
            String.join " "
                [ "("
                , showTerm firstTerm
                , showNumericalOperator operator
                , showNumericalExpression otherExpression
                , ")"
                ]


{-| Turn a `VariableAccessor` into a `String`
-}
showVariableAccessor : VariableAccessor -> String
showVariableAccessor variableAccessor =
    variableAccessor.name
        ++ (variableAccessor.accessors
                |> List.map (\accessor -> "[" ++ showNumericalExpression accessor ++ "]")
                |> String.concat
           )



-- INTERNAL SHOW HELPERS


showFactor : Factor -> String
showFactor factor =
    case factor of
        IntFactor intFactor ->
            String.fromInt intFactor

        FloatFactor floatFactor ->
            String.fromFloat floatFactor

        StringFactor stringFactor ->
            "\"" ++ stringFactor ++ "\""

        NullFactor ->
            "null"

        NamedFactor variableAccessor ->
            viewVariableAccessor variableAccessor

        ParenthesizedFactor numericalExpression ->
            "(" ++ showNumericalExpression numericalExpression ++ ")"


viewVariableAccessor : VariableAccessor -> String
viewVariableAccessor accessor =
    accessor.name
        :: List.map
            (\idxExpression -> "[" ++ showNumericalExpression idxExpression ++ "]")
            accessor.accessors
        |> String.concat


showUnaryExpression : UnaryExpression -> String
showUnaryExpression (UnaryExpression maybeSign factor) =
    let
        signString =
            case maybeSign of
                Nothing ->
                    ""

                Just Plus ->
                    "+"

                Just Minus ->
                    "-"
    in
    signString ++ showFactor factor


showTermOperator : TermOperator -> String
showTermOperator termOperator =
    case termOperator of
        Multiplication ->
            "*"

        Division ->
            "/"

        Modulo ->
            "%"


showTerm : Term -> String
showTerm term =
    case term of
        SingleTerm unaryExpression ->
            showUnaryExpression unaryExpression

        MultipleTerms unaryExpression operator otherTerm ->
            String.join " "
                [ "("
                , showUnaryExpression unaryExpression
                , showTermOperator operator
                , showTerm otherTerm
                , ")"
                ]


showNumericalOperator : NumericalOperator -> String
showNumericalOperator numericalOperator =
    case numericalOperator of
        Addition ->
            "+"

        Subtraction ->
            "-"


showComparator : Comparator -> String
showComparator comparator =
    case comparator of
        LessThan ->
            "<"

        GreatherThan ->
            ">"

        LessThanOrEqualTo ->
            "<="

        GreatherThanOrEqualTo ->
            ">="

        Equals ->
            "=="

        Different ->
            "!="

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
    , showExpression
    )

-- EXPRESSION


type Expression
    = SingleExpression NumericalExpression
    | WithComparator NumericalExpression Comparator NumericalExpression


type Comparator
    = LessThan
    | GreatherThan
    | LessThanOrEqualTo
    | GreatherThanOrEqualTo
    | Equals
    | Different



-- NUMERICAL EXPRESSION


type NumericalExpression
    = SingleNumericalExpression Term
    | MultipleNumericalExpressions Term NumericalOperator NumericalExpression


type NumericalOperator
    = Addition
    | Subtraction



-- TERM


type Term
    = SingleTerm UnaryExpression
    | MultipleTerms UnaryExpression TermOperator Term


type TermOperator
    = Multiplication
    | Division
    | Modulo



-- UNARY EXPRESSION


type Sign
    = Plus
    | Minus


type UnaryExpression
    = UnaryExpression (Maybe Sign) Factor



-- FACTOR


type Factor
    = IntFactor Int
    | FloatFactor Float
    | StringFactor String
    | NullFactor
    | NamedFactor String (List NumericalExpression)
    | ParenthesizedFactor NumericalExpression



-- SHOW HELPERS


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

        NamedFactor name accessors ->
            let
                viewAccessor accessor =
                    "[" ++ showNumericalExpression accessor ++ "]"
            in
            name
                :: List.map viewAccessor accessors
                |> String.concat

        ParenthesizedFactor numericalExpression ->
            "(" ++ showNumericalExpression numericalExpression ++ ")"


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
                [ "{"
                , showUnaryExpression unaryExpression
                , showTermOperator operator
                , showTerm otherTerm
                , "}"
                ]


showNumericalOperator : NumericalOperator -> String
showNumericalOperator numericalOperator =
    case numericalOperator of
        Addition ->
            "+"

        Subtraction ->
            "-"


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


showExpression : Expression -> String
showExpression expression =
    case expression of
        SingleExpression numericalExpression ->
            showNumericalExpression numericalExpression

        WithComparator firstExpression comparator secondExpression ->
            String.join " "
                [ showNumericalExpression firstExpression
                , showComparator comparator
                , showNumericalExpression secondExpression
                ]

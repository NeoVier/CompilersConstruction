module CCParser exposing (CCParser, Context(..), Problem(..), deadEndsToString)

import Parser.Advanced as Parser exposing (Parser)


type alias CCParser a =
    Parser Context Problem a


type Context
    = FunctionDeclaration
    | BreakStatement
    | StatementList
    | ForStatement
    | IfStatement
    | FunctionCall
    | Allocation
    | ReturnStatement
    | ReadStatement
    | PrintStatement
    | AttributionStatement
    | VariableDeclaration
    | Factor
    | ParsingString
    | UnaryExpression
    | MultipleTerms
    | SingleTerm
    | MultipleNumericalExpressions
    | SingleNumericalExpression
    | ExpressionWithComparator
    | SingleExpression


type Problem
    = ExpectingVariableName
    | ExpectingOpenBracket
    | ExpectingCloseBracket
    | ExpectingOpenParens
    | ExpectingCloseParens
    | ExpectingOpenCurlies
    | ExpectingCloseCurlies
    | ExpectingNoStart
    | ExpectingNoSeparator
    | ExpectingNoEnd
    | ExpectingNull
    | ExpectingBase10
    | ExpectingBase10Int
    | ExpectingInt
    | ExpectingNumber
    | ExpectingCharacter Char
    | ExpectingBackSlash
    | ExpectingAnything
    | ExpectingDifferentThan (List Char)
    | ExpectingStartOfString
    | ExpectingSign
    | ExpectingTermOperator
    | ExpectingNumericalOperator
    | ExpectingComparator
    | ExpectingComma
    | ExpectingKeyword String
    | ExpectingFactor


removeDuplicateDeadEnds : List (Parser.DeadEnd Context Problem) -> List (Parser.DeadEnd Context Problem)
removeDuplicateDeadEnds deadEnds =
    List.foldr
        (\deadEnd currDeadEnds ->
            if
                List.any
                    (\d ->
                        (d.problem == deadEnd.problem)
                            || (List.head d.contextStack == List.head deadEnd.contextStack)
                    )
                    currDeadEnds
            then
                currDeadEnds

            else
                deadEnd :: currDeadEnds
        )
        []
        deadEnds


deadEndsToString : List (Parser.DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
    let
        deadEnds_ =
            removeDuplicateDeadEnds deadEnds

        errorMessage =
            if List.length deadEnds_ == 1 then
                "I got an error!"

            else
                "I got a few errors!"
    in
    ((errorMessage :: List.map deadEndToString deadEnds_)
        |> String.join "\n\n"
    )
        ++ "\n\n"


deadEndToString : Parser.DeadEnd Context Problem -> String
deadEndToString deadEnd =
    let
        contextStack =
            deadEnd.contextStack
                |> List.head
                |> Maybe.map
                    (\context ->
                        contextToString context.context
                            ++ " ("
                            ++ String.fromInt context.row
                            ++ ":"
                            ++ String.fromInt context.col
                            ++ ")"
                    )
                |> Maybe.withDefault ""
    in
    "I was expecting to see "
        ++ problemToString deadEnd.problem
        ++ ".\nIf it helps, I was in the middle of looking at "
        ++ contextStack


problemToString : Problem -> String
problemToString problem =
    case problem of
        ExpectingVariableName ->
            "a variable name"

        ExpectingOpenBracket ->
            "an open bracket (`[`)"

        ExpectingCloseBracket ->
            "a closing bracket (`]`)"

        ExpectingOpenParens ->
            "an opening parens (`(`)"

        ExpectingCloseParens ->
            "a closing parens (`)`)"

        ExpectingOpenCurlies ->
            "an opening curly brackets (`{`)"

        ExpectingCloseCurlies ->
            "a closing curly brackets (`}`)"

        ExpectingNoStart ->
            "nothing before this sequence"

        ExpectingNoSeparator ->
            "nothing separating items"

        ExpectingNoEnd ->
            "nothing at the end of this sequence"

        ExpectingNull ->
            "the keyword null"

        ExpectingBase10 ->
            "a number in base 10"

        ExpectingBase10Int ->
            "an int in base 10"

        ExpectingInt ->
            "an int"

        ExpectingNumber ->
            "an int or float"

        ExpectingCharacter char ->
            "this character: `" ++ String.fromChar char ++ "`"

        ExpectingBackSlash ->
            "a back slash (`\\`)"

        ExpectingAnything ->
            "anything"

        ExpectingDifferentThan characters ->
            "something other than "
                ++ (characters
                        |> List.map (\char -> "`" ++ String.fromChar char ++ "`")
                        |> List.intersperse ","
                        |> String.join " "
                   )

        ExpectingStartOfString ->
            "the start of a string (`\"`)"

        ExpectingSign ->
            "a sign (`+` or `-`)"

        ExpectingTermOperator ->
            "a term operator (`*`, `/` or `%`)"

        ExpectingNumericalOperator ->
            "a numerical operator (`+` or `-`)"

        ExpectingComparator ->
            "a comparator (`<=`, `>=`, `<`, `>`, `==` or `!=`)"

        ExpectingComma ->
            "a comma (`,`)"

        ExpectingKeyword keyword ->
            "the keyword `" ++ keyword ++ "`"

        ExpectingFactor ->
            "a factor (this could be a number, a string, `null`, accessing a variable or a parenthesized numerical expression)"


contextToString : Context -> String
contextToString context =
    case context of
        FunctionDeclaration ->
            "a function declaration"

        BreakStatement ->
            "a break statement"

        StatementList ->
            "a list of statements"

        ForStatement ->
            "a for statement"

        IfStatement ->
            "a if statement"

        FunctionCall ->
            "a function call"

        Allocation ->
            "an allocation"

        ReturnStatement ->
            "a return statement"

        ReadStatement ->
            "a read statement"

        PrintStatement ->
            "a print statement"

        AttributionStatement ->
            "an attribution statement"

        VariableDeclaration ->
            "a variable declaration"

        Factor ->
            "a factor (number, string, null or parenthesized expression)"

        ParsingString ->
            "a string"

        UnaryExpression ->
            "a unary expression (an expression with a + or - sign)"

        MultipleTerms ->
            "a list of terms"

        SingleTerm ->
            "a term"

        MultipleNumericalExpressions ->
            "a list of numerical expressions"

        SingleNumericalExpression ->
            "a numerical expression"

        ExpressionWithComparator ->
            "a two expressions being compared"

        SingleExpression ->
            "an expression"

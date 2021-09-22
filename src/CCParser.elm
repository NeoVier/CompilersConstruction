{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   August 2021
-}


module CCParser exposing (CCParser, Context(..), Problem(..), Range, deadEndsToString)

import Parser.Advanced as Parser exposing (Parser)


type alias CCParser a =
    Parser Context Problem a


type alias Range =
    { start : ( Int, Int ), end : ( Int, Int ) }


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
    | Statement
    | VariableType
    | VariableName
    | Expression
    | NumericalExpression


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
    | ExpectingStatement
    | ExpectingIf
    | ExpectingFor
    | ExpectingStatementList
    | ExpectingVariableType


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
        |> List.filter
            (\deadEnd ->
                let
                    firstContext =
                        List.head deadEnd.contextStack |> Maybe.map .context

                    insideOfStatement =
                        List.drop 1 deadEnd.contextStack
                            |> List.head
                            |> Maybe.map
                                (\context ->
                                    case context.context of
                                        Statement ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Maybe.withDefault False
                in
                case deadEnd.problem of
                    ExpectingIf ->
                        not ((firstContext == Just IfStatement) && insideOfStatement)

                    ExpectingFor ->
                        not ((firstContext == Just ForStatement) && insideOfStatement)

                    ExpectingStatementList ->
                        not ((firstContext == Just StatementList) && insideOfStatement)

                    ExpectingVariableType ->
                        not ((firstContext == Just VariableType) && insideOfStatement)

                    ExpectingVariableName ->
                        not ((firstContext == Just VariableName) && insideOfStatement)

                    _ ->
                        True
            )


deadEndsToString : List (Parser.DeadEnd Context Problem) -> String
deadEndsToString deadEnds =
    let
        deadEnds_ =
            removeDuplicateDeadEnds deadEnds

        errorMessage =
            if List.length deadEnds_ == 1 then
                "I got an error!"

            else if List.length deadEnds_ > 1 then
                "I got a few errors!"

            else
                "I got 0 errors, but something went wrong"
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
                |> List.take 2
                |> List.map
                    (\context ->
                        contextToString context.context
                            ++ " ("
                            ++ String.fromInt context.row
                            ++ ":"
                            ++ String.fromInt context.col
                            ++ ")"
                    )
                |> String.join " inside of "
    in
    "I was expecting to see "
        ++ problemToString deadEnd.problem
        ++ " on ("
        ++ String.fromInt deadEnd.row
        ++ ":"
        ++ String.fromInt deadEnd.col
        ++ ").\n"
        ++ (if List.isEmpty deadEnd.contextStack then
                ""

            else
                "If it helps, I was in the middle of looking at " ++ contextStack
           )


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

        ExpectingStatement ->
            "a statement, which could be any of these: \n"
                ++ ([ "a variable declaration"
                    , "an attribution statement"
                    , "a print statement"
                    , "a read statement"
                    , "a return statement"
                    , "an if statement"
                    , "a for statement"
                    , "a list of statements"
                    , "a break statement"
                    , "a semicolon (`;`)"
                    ]
                        |> List.map (\description -> "\t" ++ description)
                        |> String.join "\n"
                   )

        ExpectingIf ->
            "an if statement"

        ExpectingFor ->
            "a for statement"

        ExpectingStatementList ->
            "a list of statements"

        ExpectingVariableType ->
            "a variable type (`int`, `float` or `string`)"


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
            "an if statement"

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

        Statement ->
            "a statement"

        VariableType ->
            "a variable type"

        VariableName ->
            "a variable name"

        Expression ->
            "an expression"

        NumericalExpression ->
            "a numerical expression"

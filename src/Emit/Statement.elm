{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.Statement exposing (emit, fromIf)

import CCParser
import Emit.Expression
import Emit.State as State exposing (State)
import Syntax.Expression
import Syntax.Statement


emit : Syntax.Statement.Statement -> State -> State
emit statement state =
    case statement of
        Syntax.Statement.VariableDeclaration declaration ->
            fromDeclaration declaration state

        Syntax.Statement.AttributionStatement attribution ->
            fromAttribution attribution state

        Syntax.Statement.PrintStatement expression ->
            fromPrint expression state

        Syntax.Statement.ReadStatement variableAccessor ->
            fromRead variableAccessor state

        Syntax.Statement.ReturnStatement range ->
            fromReturn range state

        Syntax.Statement.IfStatement if_ ->
            fromIf if_ state

        Syntax.Statement.ForStatement for ->
            fromFor for state

        Syntax.Statement.StatementBlock statements ->
            fromBlock statements state

        Syntax.Statement.BreakStatement range ->
            fromBreak range state

        Syntax.Statement.Semicolon ->
            fromSemicolon state


fromDeclaration : Syntax.Statement.Declaration -> State -> State
fromDeclaration declaration state =
    let
        stateType =
            case declaration.type_ of
                Syntax.Statement.IntVariable ->
                    State.IntVariable

                Syntax.Statement.FloatVariable ->
                    State.FloatVariable

                Syntax.Statement.StringVariable ->
                    State.StringVariable
    in
    State.addVariable { type_ = stateType, name = declaration.name } state


fromAttribution : Syntax.Statement.Attribution -> State -> State
fromAttribution attribution state =
    let
        ( afterAccessors, accessorString ) =
            Emit.Expression.fromVariableAccessor attribution.variableAccessor state

        ( afterValue, valueString ) =
            case attribution.value of
                Syntax.Statement.ExpressionAttribution expression ->
                    let
                        state_ =
                            Emit.Expression.emit expression afterAccessors
                    in
                    ( state_, State.lastTemporaryVariable state_ )

                Syntax.Statement.AllocationExpression _ ->
                    Debug.todo "Implement allocation expression"

                Syntax.Statement.FunctionCallAttribution functionCall ->
                    ( afterAccessors
                    , functionCall.functionName
                        ++ "("
                        ++ (functionCall.parameters
                                |> String.join ", "
                           )
                        ++ ")"
                    )

        code =
            "{{ACCESSOR}} = {{VALUE}}"
                |> String.replace "{{ACCESSOR}}" accessorString
                |> String.replace "{{VALUE}}" valueString
    in
    State.addLine code afterValue


fromPrint : Syntax.Expression.Expression -> State -> State
fromPrint expression state =
    let
        afterExpression =
            Emit.Expression.emit expression state

        code =
            "print {{EXPRESSION}}"
                |> String.replace "{{EXPRESSION}}" (State.lastTemporaryVariable afterExpression)
    in
    State.addLine code afterExpression


fromRead : Syntax.Expression.VariableAccessor -> State -> State
fromRead accessor state =
    let
        ( afterAccessors, accessorString ) =
            Emit.Expression.fromVariableAccessor accessor state
    in
    State.addLine ("read " ++ accessorString) afterAccessors


fromReturn : CCParser.Range -> State -> State
fromReturn range state =
    case State.currentContext state of
        Just (State.FunctionContext exitLabel) ->
            State.addLineWithLabel
                { code = "goto {{LABEL}}", labelPlaceholder = "{{LABEL}}" }
                exitLabel
                state

        _ ->
            State.raiseError
                { range = range
                , message = "The `return` keyword can only be used inside functions! Otherwise, where am I supposed to return to?"
                }
                state


{-| Given some code like

    if (5 > 3) {
        ...
    } else {
        ...
    }

generates code like

    $temp$ = 5 > 3
    if False $temp$ goto AFTER_IF
    ...
    goto AFTER_ELSE
    AFTER_IF:
    ...
    AFTER_ELSE:

-}
fromIf : Syntax.Statement.If -> State -> State
fromIf { condition, body, elseBody } state =
    let
        afterCondition =
            Emit.Expression.emit condition state

        ( afterIf, afterIfLabel ) =
            State.createLabel afterCondition
                |> (\( state_, label_ ) ->
                        let
                            ifLine =
                                "if False {{CONDITION}} goto {{AFTER_IF_LABEL}}"
                                    |> String.replace "{{CONDITION}}"
                                        (State.lastTemporaryVariable state_)
                        in
                        ( state_
                            |> State.addLineWithLabel
                                { code = ifLine, labelPlaceholder = "{{AFTER_IF_LABEL}}" }
                                label_
                            |> State.enterContext State.IfContext
                        , label_
                        )
                   )

        afterBody =
            emit body afterIf
                |> State.leaveContext
    in
    case elseBody of
        Nothing ->
            afterBody
                |> State.addLabel afterIfLabel

        Just else_ ->
            let
                ( afterElse, afterElseLabel ) =
                    State.createLabel afterBody
                        |> (\( state_, label_ ) ->
                                ( State.addLineWithLabel
                                    { code = "goto {{AFTER_ELSE_BODY}}", labelPlaceholder = "{{AFTER_ELSE_BODY}}" }
                                    label_
                                    state_
                                , label_
                                )
                           )
                        |> Tuple.mapFirst
                            (State.addLabel afterIfLabel
                                >> State.enterContext State.ElseContext
                                >> emit else_
                                >> State.leaveContext
                            )
            in
            afterElse
                |> State.addLabel afterElseLabel


{-| Given some code like

    for (i = 0; i < 10; i = i + 1) {
        ...
    }

generates code like

    i = 0

    CONDITION_START:
    if i < 10 goto FOR_END
    ...
    i = i + 1
    goto CONDITION_START

    FOR_END:

-}
fromFor : Syntax.Statement.For -> State -> State
fromFor for state =
    let
        ( withCondition, startLabel, endLabel ) =
            fromAttribution for.attribution state
                |> State.createLabel
                |> (\( state_, label_ ) ->
                        let
                            ( endState_, endLabel_ ) =
                                State.createLabel state_
                        in
                        ( State.addLabel label_ endState_
                            |> Emit.Expression.emit for.condition
                        , label_
                        , endLabel_
                        )
                   )

        conditionCode =
            "if {{CONDITION}} goto {{FOR_END}}"
                |> String.replace "{{CONDITION}}" (State.lastTemporaryVariable withCondition)
    in
    withCondition
        |> State.addLineWithLabel
            { code = conditionCode, labelPlaceholder = "{{FOR_END}}" }
            endLabel
        |> State.enterContext (State.ForContext endLabel)
        |> emit for.body
        |> fromAttribution for.increment
        |> State.addLineWithLabel
            { code = "goto {{FOR_CONDITION}}", labelPlaceholder = "{{FOR_CONDITION}}" }
            startLabel
        |> State.leaveContext
        |> State.addLabel endLabel


fromBlock : Syntax.Statement.StatementList -> State -> State
fromBlock { firstStatement, otherStatements } state =
    List.foldl (\currStatement currState -> emit currStatement currState)
        (State.enterContext State.BlockContext state)
        (firstStatement :: otherStatements)
        |> State.leaveContext


fromBreak : CCParser.Range -> State -> State
fromBreak range state =
    case State.currentContext state of
        Just (State.ForContext exitLabel) ->
            State.addLineWithLabel
                { code = "goto {{LABEL}}", labelPlaceholder = "{{LABEL}}" }
                exitLabel
                state

        _ ->
            State.raiseError
                { range = range
                , message = "The `break` keyword can only be used inside a `for` loop"
                }
                state


fromSemicolon : State -> State
fromSemicolon state =
    state

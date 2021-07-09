{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   July 2021
-}


module Syntax.Symbol exposing (Table, encodeTable, tableFromProgram)

import Dict exposing (Dict)
import Json.Encode as Encode
import List.Extra
import Parser.Statement exposing (functionDeclaration)
import Syntax.Program as Program
import Syntax.Statement as Statement


{-| We need a `Dict` so we can store qualified names (i.e. `function1.A`)
-}
type alias Table =
    Dict String TableEntry


{-| A symbol needs to go in the table if it's one of these variants
-}
type TableEntry
    = VariableDeclaration Statement.Declaration
    | FunctionDeclaration Statement.FunctionDeclaration
    | FunctionParameter Statement.FunctionParameter


{-| Turn a `Table` into a JSON value so we can send it for JS to print it nicely
-}
encodeTable : Table -> Encode.Value
encodeTable table =
    table
        |> Dict.map
            (\name tableEntry ->
                case tableEntry of
                    VariableDeclaration declaration ->
                        { name = name
                        , type_ = Statement.showVariableType declaration.type_
                        , dimmensions =
                            declaration.dimmensions
                                |> List.map
                                    (\dimmension -> "[" ++ String.fromInt dimmension ++ "]")
                                |> String.concat
                        }

                    FunctionDeclaration functionDeclaration ->
                        { name = name
                        , type_ = "function"
                        , dimmensions =
                            functionDeclaration.parameters
                                |> List.map (.type_ >> Statement.showVariableType)
                                |> String.join ", "
                                |> (\parameters -> "(" ++ parameters ++ ")")
                        }

                    FunctionParameter functionParameter ->
                        { name = name
                        , type_ = Statement.showVariableType functionParameter.type_
                        , dimmensions = ""
                        }
            )
        |> Dict.values
        |> Encode.list
            (\value ->
                Encode.object
                    [ ( "name", Encode.string value.name )
                    , ( "type", Encode.string value.type_ )
                    , ( "dimmensions", Encode.string value.dimmensions )
                    ]
            )


{-| Extract a `Table` out of a `Program`
-}
tableFromProgram : Program.Program -> Table
tableFromProgram program =
    case program of
        Program.SingleStatement statement ->
            Dict.empty
                |> withStatement statement []

        Program.FunctionList functionList ->
            List.foldl
                (\currFunction currTable ->
                    currTable
                        |> withFunction currFunction
                )
                Dict.empty
                functionList



-- INTERNAL HELPERS


withStatement : Statement.Statement -> List String -> Table -> Table
withStatement statement currentName table =
    case statement of
        Statement.VariableDeclaration declaration ->
            Dict.insert
                (currentName ++ [ declaration.name ] |> String.join ".")
                (VariableDeclaration declaration)
                table

        Statement.StatementBlock statementList ->
            (statementList.firstStatement :: statementList.otherStatements)
                |> List.Extra.indexedFoldl
                    (\currIndex currStatement currTable ->
                        currTable
                            |> withStatement currStatement (currentName ++ [ String.fromInt currIndex ])
                    )
                    table

        _ ->
            table


withFunction : Statement.FunctionDeclaration -> Table -> Table
withFunction functionDeclaration table =
    table
        |> Dict.insert functionDeclaration.name (FunctionDeclaration functionDeclaration)
        |> (\tableWithFunction ->
                List.foldl
                    (\currParameter currTable ->
                        currTable
                            |> withFunctionParameter currParameter [ functionDeclaration.name ]
                    )
                    tableWithFunction
                    functionDeclaration.parameters
           )
        |> (\tableWithParameters ->
                (functionDeclaration.body.firstStatement :: functionDeclaration.body.otherStatements)
                    |> List.foldl
                        (\currStatement currTable ->
                            currTable
                                |> withStatement currStatement [ functionDeclaration.name ]
                        )
                        tableWithParameters
           )


withFunctionParameter : Statement.FunctionParameter -> List String -> Table -> Table
withFunctionParameter functionParameter currentName table =
    table
        |> Dict.insert
            (currentName ++ [ functionParameter.name ] |> String.join ".")
            (FunctionParameter functionParameter)

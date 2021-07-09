{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Syntax.Program exposing (Program(..), show)

{-| The most high-level point of an LCC application. It encompasses the entire
file, and contains every single definition of the program
-}

import Syntax.Statement as Statement


{-| A `Program` consists of either a `SingleStatement`, or a list of
`FunctionDeclarations`
-}
type Program
    = SingleStatement Statement.Statement
    | FunctionList (List Statement.FunctionDeclaration)


{-| Turn a `Program` into a `String`
-}
show : Program -> String
show program =
    case program of
        SingleStatement statement ->
            Statement.show statement

        FunctionList functionList ->
            functionList
                |> List.map Statement.showFunctionDeclaration
                |> String.join "\n\n"

{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Syntax.Program exposing (Program(..))

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

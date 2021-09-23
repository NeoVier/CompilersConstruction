{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.Program exposing (emit)

import Emit.State exposing (State)
import Emit.Statement
import Syntax.Program


emit : Syntax.Program.Program -> State -> State
emit program state =
    case program of
        Syntax.Program.SingleStatement statement ->
            Emit.Statement.emit statement state

        Syntax.Program.FunctionList functionDeclarations ->
            List.foldl
                (\currFunctionDeclaration currState ->
                    Emit.Statement.fromFunctionDeclaration currFunctionDeclaration currState
                )
                state
                functionDeclarations

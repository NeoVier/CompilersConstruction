{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit exposing (..)

import Emit.Expression
import Emit.State
import Parser.Advanced
import Parser.Expression
import Syntax.Expression


fromString : String -> String
fromString code =
    Parser.Advanced.run Parser.Expression.expression code
        |> Result.map (\expr -> emit expr)
        |> Result.withDefault "SOMETHING WRONG"


emit : Syntax.Expression.Expression -> String
emit expression =
    Emit.State.initialState
        |> Emit.Expression.emit expression
        |> Emit.State.code

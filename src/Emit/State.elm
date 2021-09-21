{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   September 2021
-}


module Emit.State exposing (State, initialState, temporaryVariable)


type alias State =
    { code : List String
    , lastTemporaryIndex : Int
    }


initialState : State
initialState =
    { code = []
    , lastTemporaryIndex = -1
    }


temporaryVariable : Int -> String
temporaryVariable index =
    "$temp$" ++ String.fromInt index

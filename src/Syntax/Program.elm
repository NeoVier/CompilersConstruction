module Syntax.Program exposing (Program(..))

import Syntax.Statement as Statement


type Program
    = SingleStatement Statement.Statement
    | FunctionList (List Statement.FunctionDeclaration)

module Parser.Program exposing (program)

import Parser exposing ((|.), (|=), Parser)
import Parser.Statement as Statement
import Syntax.Program as Program


program : Parser Program.Program
program =
    Parser.oneOf
        [ Statement.statement
            |> Parser.map Program.SingleStatement
        , Parser.succeed Program.FunctionList
            |= Parser.sequence
                { start = ""
                , separator = ""
                , end = ""
                , spaces = Parser.spaces
                , item = Statement.functionDeclaration
                , trailing = Parser.Optional
                }
        ]

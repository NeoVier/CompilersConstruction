{- UFSC - CTC - INE5426 Construcao de Compiladores
   Henrique da Cunha Buss
   June 2021
-}


module Parser.Program exposing (program)

import CCParser exposing (CCParser)
import Parser.Advanced as Parser exposing ((|.), (|=))
import Parser.Statement as Statement
import Syntax.Program as Program


program : CCParser Program.Program
program =
    Parser.oneOf
        [ Statement.statement
            |> Parser.map Program.SingleStatement
        , Parser.succeed Program.FunctionList
            |= Parser.sequence
                { start = Parser.Token "" CCParser.ExpectingNoStart
                , separator = Parser.Token "" CCParser.ExpectingNoSeparator
                , end = Parser.Token "" CCParser.ExpectingNoEnd
                , spaces = Parser.spaces
                , item = Statement.functionDeclaration
                , trailing = Parser.Optional
                }
        ]

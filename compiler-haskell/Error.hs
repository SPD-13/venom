module Error where

import Data.List

import qualified Position as P

data ErrorPosition
    = Position P.Position
    | EOF

data Error = Error
    { message :: String
    , position :: ErrorPosition
    }

printError :: String -> Error -> String
printError file error =
    let fileLines = lines file
        (line, column) = case position error of
            Position pos -> (P.line pos, P.column pos)
            EOF ->
                let line = genericLength fileLines
                    column = genericLength(genericIndex fileLines (line - 1)) + 1
                in (line, column)
        location = "Error at line " ++ show line ++ ", column " ++ show column ++ ":"
        errorLine = '|' : ' ' : genericIndex fileLines (line - 1)
        indicator = '|' : genericReplicate column ' ' ++ "^"
    in unlines ["", location, message error, "|", errorLine, indicator]

printErrors :: String -> [Error] -> String
printErrors file errors =
    concatMap (printError file) errors

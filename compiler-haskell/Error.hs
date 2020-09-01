module Error where

import Data.List

import qualified Position as P

data Error = Error
    { message :: String
    , position :: P.Position
    }

printError :: String -> Error -> String
printError file error =
    let pos = position error
        line = P.line pos
        column = P.column pos
        location = "Error at line " ++ show line ++ ", column " ++ show column ++ ":"
        errorLine = '|' : ' ' : genericIndex (lines file) (line - 1)
        indicator = '|' : genericReplicate column ' ' ++ "^"
    in unlines ["", location, message error, "|", errorLine, indicator]

printErrors :: String -> [Error] -> String
printErrors file errors =
    concatMap (printError file) errors

module AST where

import Operator

data AST
    = Binary AST Operator AST
    | Integer Integer
    | None
    deriving Show

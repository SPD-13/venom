module AST where

import Operator

data AST
    = Expression Expression
    deriving Show

data Expression
    = Binary Expression Operator Expression
    | Integer Integer
    | None
    deriving Show

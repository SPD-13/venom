module AST where

import Operator

data AST
    = Bindings [Binding]
    deriving Show

data Binding
    = Binding String Expression
    deriving Show

data Expression
    = Binary Expression Operator Expression
    | Integer Integer
    | Value String
    | None
    deriving Show

module AST where

import Operator

data AST
    = Bindings [Binding]
    deriving Show

data Binding
    = Binding String Expression
    deriving Show

data Expression
    = Let [Binding] Expression
    | If Expression Expression Expression
    | Binary Expression Operator Expression
    | Literal Concrete
    | Identifier String
    | None
    deriving Show

data Concrete
    = Integer Integer
    | Bool Bool
    deriving (Eq, Show)

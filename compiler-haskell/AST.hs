module AST where

import Operator

data AST
    = Bindings [Binding]
    deriving Show

data Binding
    = Function String [String] Expression
    | Constant String Expression
    deriving Show

data Expression
    = Let [Binding] Expression
    | If Expression Expression Expression
    | Binary Expression Operator Expression
    | Call Expression [Expression]
    | Literal Concrete
    | Identifier String
    | None
    deriving Show

data Concrete
    = Integer Integer
    | Bool Bool
    deriving (Eq, Show)

module AST where

import Operator

data AST
    = Bindings [Binding]
    deriving Show

data Binding
    = Binding String Expression
    deriving (Eq, Show)

data Expression
    = Let [Binding] Expression
    | If Expression Expression Expression
    | Binary Expression Operator Expression
    | Call Expression [Expression]
    | Literal Literal
    | Identifier String
    | None
    deriving (Eq, Show)

data Literal
    = Integer Integer
    | Bool Bool
    | Function [String] Expression
    deriving (Eq, Show)

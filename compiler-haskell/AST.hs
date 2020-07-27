module AST where

import Operator

data AST
    = Bindings [Binding]
    deriving Show

data Binding
    = FunctionDeclaration String [String] Expression
    | Constant String Expression
    deriving (Eq, Show)

data Expression
    = Let [Binding] Expression
    | If Expression Expression Expression
    | Binary Expression Operator Expression
    | Call Expression [Expression]
    | Literal Concrete
    | Identifier String
    | None
    deriving (Eq, Show)

data Concrete
    = Integer Integer
    | Bool Bool
    | Function [String] Expression
    deriving (Eq, Show)

module AST where

data AST
    = Binary AST Operator AST
    | Literal

data Operator
    = Equality
    | Inequality
    | Plus
    | Minus

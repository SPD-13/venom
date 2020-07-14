module Operator where

data Operator
    = Equality -- ==
    | Inequality -- /=
    | Plus -- +
    | Minus -- -
    | Modulo -- %
    | Pipe -- |>
    | ReversePipe -- <|
    | PartialPipe -- !>
    | ReversePartialPipe -- <!
    | Or -- ||
    | And -- &&
    deriving (Show, Eq)

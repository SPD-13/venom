module Parser where

import Token
import AST
import Operator

parse :: [Token] -> AST
parse tokens =
    let (ast, _) = expression tokens
    in ast

expression :: [Token] -> (AST, [Token])
expression tokens = mapFirst Expression $ equality tokens

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) = (f a, b)

makeBinaryParser :: ([Token] -> (Expression, [Token])) -> [Operator] -> ([Token] -> (Expression, [Token]))
makeBinaryParser operandParser operators =
    let
        recurse result@(left, rest) =
            case map tokenType $ peek rest of
                [Operator op] ->
                    if op `elem` operators then
                        let (right, newRest) = operandParser $ tail rest
                        in recurse (Binary left op right, newRest)
                    else
                        result
                _ -> result
    in recurse . operandParser

equality :: [Token] -> (Expression, [Token])
equality = makeBinaryParser addition [Equality, Inequality]

addition :: [Token] -> (Expression, [Token])
addition = makeBinaryParser primary [Plus, Minus]

primary :: [Token] -> (Expression, [Token])
primary [] = (None, [])
primary (head:rest) =
    case tokenType head of
        (Token.Integer integer) -> (AST.Integer integer, rest)
        _ -> (None, [])

peek :: [Token] -> [Token]
peek = take 1

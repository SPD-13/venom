module Parser where

import Token
import AST

parse :: [Token] -> AST
parse tokens =
    let (ast, _) = expression tokens
    in ast

expression :: [Token] -> (AST, [Token])
expression tokens =
    equality tokens

equality :: [Token] -> (AST, [Token])
equality tokens =
    let result@(left, rest) = addition tokens
        nextToken = map tokenType $ peek rest
    in
        if nextToken `elem` [[Token.Equality], [Token.Inequality]] then
            let operator = if nextToken == [Token.Equality] then AST.Equality else AST.Inequality
                (right, newRest) = equality $ tail rest
            in (Binary left operator right, newRest)
        else
            result

addition :: [Token] -> (AST, [Token])
addition tokens =
    let result@(left, rest) = primary tokens
        nextToken = map tokenType $ peek rest
    in
        if nextToken `elem` [[Token.Plus], [Token.Minus]] then
            let operator = if nextToken == [Token.Plus] then AST.Plus else AST.Minus
                (right, newRest) = addition $ tail rest
            in (Binary left operator right, newRest)
        else
            result

primary :: [Token] -> (AST, [Token])
primary tokens =
    (Literal, tokens)

peek :: [Token] -> [Token]
peek [] = []
peek (head:_) = [head]

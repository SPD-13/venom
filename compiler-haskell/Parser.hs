module Parser where

import Token
import AST
import Operator

parse :: [Token] -> AST
parse tokens =
    let (ast, rest) = mapFirst Bindings $ bindings tokens
    in
        if null rest then
            ast
        else
            ast -- Error

bindings :: [Token] -> ([Binding], [Token])
bindings [] = ([], [])
bindings tokens@(head:rest) =
    case tokenType head of
        (Token.Identifier identifier) ->
            case peek rest of
                [LeftParen] -> function identifier $ tail rest
                [Equals] -> constant identifier $ tail rest
                _ -> ([], [])
        _ -> ([], tokens)

function identifier tokens =
    let (params, rest) = parameters tokens
    in case peek rest of
        [Equals] ->
            let (expr, newTokens) = expression $ tail rest
                binding = Binding identifier (Literal (Function params expr)) TUndefined
            in mapFirst (binding:) $ bindings newTokens
        _ -> ([], [])

parameters :: [Token] -> ([String], [Token])
parameters tokens =
    case peek tokens of
        [RightParen] -> ([], tail tokens)
        _ -> recurseParameters tokens

recurseParameters tokens =
    let (head, rest) = consume tokens
    in case head of
        [Token.Identifier identifier] ->
            case peek rest of
                [Comma] ->
                    let (params, newRest) = recurseParameters $ tail rest
                    in (identifier : params, newRest)
                [RightParen] -> ([identifier], tail rest)
                _ -> ([], [])
        _ -> ([], [])

constant identifier tokens =
    let (expr, newTokens) = expression tokens
        binding = Binding identifier expr TUndefined
    in mapFirst (binding:) $ bindings newTokens

expression :: [Token] -> (Expression, [Token])
expression tokens = logicOr tokens

mapFirst :: (a -> b) -> (a, c) -> (b, c)
mapFirst f (a, b) = (f a, b)

makeBinaryParser :: ([Token] -> (Expression, [Token])) -> [Operator] -> ([Token] -> (Expression, [Token]))
makeBinaryParser operandParser operators =
    let
        recurse result@(left, rest) =
            case peek rest of
                [Operator op] ->
                    if op `elem` operators then
                        let (right, newRest) = operandParser $ tail rest
                        in recurse (Binary left op right, newRest)
                    else
                        result
                _ -> result
    in recurse . operandParser

logicOr = makeBinaryParser logicAnd [Or]
logicAnd = makeBinaryParser equality [And]
equality = makeBinaryParser addition [Equality, Inequality]
addition = makeBinaryParser multiplication [Plus, Minus]
multiplication = makeBinaryParser call [Times]

call :: [Token] -> (Expression, [Token])
call = recurseCall . primary

recurseCall :: (Expression, [Token]) -> (Expression, [Token])
recurseCall result@(callee, tokens) =
    case peek tokens of
        [LeftParen] ->
            let (args, rest) = arguments $ tail tokens
                currentCall = Call callee args
            in recurseCall (currentCall, rest)
        _ -> result

arguments :: [Token] -> ([Expression], [Token])
arguments tokens =
    case peek tokens of
        [RightParen] -> ([], tail tokens)
        _ -> recurseArguments tokens

recurseArguments :: [Token] -> ([Expression], [Token])
recurseArguments tokens =
    let (expr, rest) = expression tokens
    in case peek rest of
        [Comma] ->
            let (args, newRest) = recurseArguments $ tail rest
            in (expr : args, newRest)
        [RightParen] -> ([expr], tail rest)
        _ -> ([], [])

primary :: [Token] -> (Expression, [Token])
primary [] = (None, [])
primary (head:rest) =
    case tokenType head of
        Token.Let ->
            let (local, newRest) = bindings rest
            in
                if peek newRest == [In] then
                    let (expr, finalRest) = expression $ tail newRest
                    in (AST.Let local expr, finalRest)
                else
                    (None, [])
        Token.If ->
            let (condition, newRest) = expression rest
            in
                if peek newRest == [Then] then
                    let (trueValue, trueRest) = expression $ tail newRest
                    in
                        if peek trueRest == [Else] then
                            let (falseValue, falseRest) = expression $ tail trueRest
                            in (AST.If condition trueValue falseValue, falseRest)
                        else
                            (None, [])
                else
                    (None, [])
        LeftParen ->
            let (expr, newRest) = expression rest
            in
                if peek newRest == [RightParen] then
                    (expr, tail newRest)
                else
                    (None, [])
        (Token.Integer integer) -> (Literal $ AST.Integer integer, rest)
        (Token.Identifier identifier) -> (AST.Identifier identifier, rest)
        _ -> (None, [])

peek :: [Token] -> [TokenType]
peek = map tokenType . take 1

consume :: [Token] -> ([TokenType], [Token])
consume [] = ([], [])
consume (head:rest) = ([tokenType head], rest)

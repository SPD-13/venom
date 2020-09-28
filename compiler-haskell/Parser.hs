{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad.State.Lazy

import AST
import Error
import Operator
import Token

data ParserState = ParserState
    { remainingTokens :: [Token]
    , errors :: [Error]
    }

parse :: [Token] -> Either [Error] AST
parse tokens =
    let initialState = ParserState tokens []
        (ast, finalState) = runState parseTokens initialState
    in if null (errors finalState) then
        Right ast
    else
        Left $ reverse $ errors finalState

advance :: Int -> State ParserState ()
advance n = modify $ \s -> s { remainingTokens = drop n (remainingTokens s) }

peek :: State ParserState [TokenType]
peek = fmap (map tokenType . take 1) $ gets remainingTokens

consume :: State ParserState ([TokenType], [Token])
consume = do
    next <- fmap (take 1) $ gets remainingTokens
    advance 1
    return (map tokenType next, next)

reportError :: String -> [Token] -> State ParserState ()
reportError message maybeToken =
    let pos = case maybeToken of
            [] -> EOF
            (token:_) -> Position $ Token.position token
        err = Error message pos
    in modify $ \s -> s { remainingTokens = [], errors = err : errors s }

parseTokens :: State ParserState AST
parseTokens = do
    declarations <- bindings
    tokens <- gets remainingTokens
    unless (null tokens) $ reportError "Unexpected token after top-level declarations" tokens
    return $ Bindings declarations

bindings :: State ParserState [Binding]
bindings = peek >>= \case
    [Token.Identifier identifier] -> do
        advance 1
        (next, token) <- consume
        case next of
            [Equals] -> constant identifier
            [LeftParen] -> function identifier
            _ -> do
                reportError "Expecting '=' or '(' after identifier in binding" token
                return []
    _ -> return []

function :: String -> State ParserState [Binding]
function identifier = do
    params <- parameters
    (next, token) <- consume
    case next of
        [Colon] -> do
            (next, token) <- consume
            case next of
                [BaseType annotation] -> do
                    (next, token) <- consume
                    case next of
                        [Equals] -> do
                            expr <- expression
                            let binding = Binding identifier (Literal (Lambda [] (Function params annotation expr))) TUndefined
                            otherBindings <- bindings
                            return $ binding : otherBindings
                        _ -> do
                            reportError "Expecting '=' after return type in function definition" token
                            return []
                _ -> do
                    reportError "Invalid type annotation" token
                    return []
        _ -> do
            reportError "Expecting annotation of return type after parameters in function definition" token
            return []

parameters :: State ParserState [(String, ExpressionType)]
parameters = peek >>= \case
    [RightParen] -> do
        advance 1
        return []
    _ -> recurseParameters

recurseParameters :: State ParserState [(String, ExpressionType)]
recurseParameters = do
    (next, token) <- consume
    case next of
        [Token.Identifier identifier] -> do
            (next, token) <- consume
            case next of
                [Colon] -> do
                    (next, token) <- consume
                    case next of
                        [BaseType annotation] -> do
                            (next, token) <- consume
                            case next of
                                [Comma] -> do
                                    params <- recurseParameters
                                    return $ (identifier, annotation) : params
                                [RightParen] -> return [(identifier, annotation)]
                                _ -> do
                                    reportError "Expecting ',' or ')' after parameter in function definition" token
                                    return []
                        _ -> do
                            reportError "Invalid type annotation" token
                            return []
                _ -> do
                    reportError "Function parameters must be followed by a type annotation" token
                    return []
        _ -> do
            reportError "Expecting identifier in parameter list" token
            return []

constant :: String -> State ParserState [Binding]
constant identifier = do
    expr <- expression
    let binding = Binding identifier expr TUndefined
    otherBindings <- bindings
    return $ binding : otherBindings

expression :: State ParserState Expression
expression = logicOr

makeBinaryParser :: State ParserState Expression -> [Operator] -> State ParserState Expression
makeBinaryParser operandParser operators =
    let recurse left = peek >>= \case
            [Operator op] | op `elem` operators -> do
                advance 1
                right <- operandParser
                recurse $ Binary left op right
            _ -> return left
    in do
        left <- operandParser
        recurse left

logicOr = makeBinaryParser logicAnd [Or]
logicAnd = makeBinaryParser equality [And]
equality = makeBinaryParser addition [Equality, Inequality]
addition = makeBinaryParser multiplication [Plus, Minus]
multiplication = makeBinaryParser call [Times]

call :: State ParserState Expression
call = do
    callee <- primary
    recurseCall callee

recurseCall :: Expression -> State ParserState Expression
recurseCall callee = peek >>= \case
    [LeftParen] -> do
        advance 1
        args <- arguments
        recurseCall $ Call callee args
    _ -> return callee

arguments :: State ParserState [Expression]
arguments = peek >>= \case
    [RightParen] -> do
        advance 1
        return []
    _ -> recurseArguments

recurseArguments :: State ParserState [Expression]
recurseArguments = do
    expr <- expression
    (next, token) <- consume
    case next of
        [Comma] -> do
            args <- recurseArguments
            return $ expr : args
        [RightParen] -> return [expr]
        _ -> do
            reportError "Expecting ',' or ')' after expression in argument list" token
            return []

primary :: State ParserState Expression
primary = do
    (next, token) <- consume
    case next of
        [Token.Let] -> do
            local <- bindings
            (next, token) <- consume
            if next == [In] then do
                expr <- expression
                return $ AST.Let local expr
            else do
                reportError "Expecting 'in' after bindings in 'let' expression" token
                return None
        [Token.If] -> do
            condition <- expression
            (next, token) <- consume
            if next == [Then] then do
                trueValue <- expression
                (next, token) <- consume
                if next == [Else] then do
                    falseValue <- expression
                    return $ AST.If condition trueValue falseValue
                else do
                    reportError "Expecting 'else' after 'then' expression in 'if' expression" token
                    return None
            else do
                reportError "Expecting 'then' after condition in 'if' expression" token
                return None
        [LeftParen] -> do
            expr <- expression
            (next, token) <- consume
            if next == [RightParen] then
                return expr
            else do
                reportError "Expecting closing ')' after expression" token
                return None
        [Token.Integer integer] -> return $ Literal $ AST.Integer integer
        [Token.Char char] -> return $ Literal $ AST.Char char
        [Token.String string] -> return $ Literal $ AST.String string
        [Token.Identifier identifier] -> return $ AST.Identifier identifier $ Token.position $ head token
        _ -> do
            reportError "Unexpected token" token
            return None

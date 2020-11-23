{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad.State.Lazy
import Data.List.NonEmpty (NonEmpty((:|)))

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
    typeDeclarations <- types
    declarations <- bindings
    tokens <- gets remainingTokens
    unless (null tokens) $ reportError "Unexpected token after top-level declarations" tokens
    return $ AST typeDeclarations declarations

types :: State ParserState [TypeDeclaration]
types = peek >>= \case
    [DataType name] -> do
        advance 1
        generics <- genericParams
        (next, token) <- consume
        case next of
            [Equals] -> typeConstructors name generics
            _ -> do
                let error = case generics of
                        [] -> "Expecting '<' or '=' after identifier in type declaration"
                        _ -> "Expecting '=' after generic parameters in type declaration"
                reportError error token
                return []
    _ -> return []

genericParams :: State ParserState [GenericParameter]
genericParams = peek >>= \case
    [LeftAngle] -> do
        advance 1
        recurseGenericParams "<"
    _ -> return []

recurseGenericParams :: String -> State ParserState [GenericParameter]
recurseGenericParams previousChar = do
    (next, token) <- consume
    case next of
        [Token.Identifier name] -> do
            (next, token) <- consume
            case next of
                [Comma] -> do
                    otherParams <- recurseGenericParams ","
                    return $ name : otherParams
                [RightAngle] -> return [name]
                _ -> do
                    reportError "Expecting ',' or '>' after identifier in list of generic parameters" token
                    return [name]
        _ -> do
            reportError ("Expecting identifier after '" ++ previousChar ++ "' in list of generic parameters") token
            return []

typeConstructors :: String -> [String] -> State ParserState [TypeDeclaration]
typeConstructors name generics = do
    maybeCons <- constructor
    case maybeCons of
        Just cons -> do
            otherCons <- otherConstructors
            otherTypes <- types
            return $ TypeDeclaration name generics (cons :| otherCons) : otherTypes
        Nothing -> return []

otherConstructors :: State ParserState [Constructor]
otherConstructors = peek >>= \case
    [Union] -> do
        advance 1
        maybeCons <- constructor
        case maybeCons of
            Just cons -> do
                otherCons <- otherConstructors
                return $ cons : otherCons
            Nothing -> return []
    _ -> return []

constructor :: State ParserState (Maybe Constructor)
constructor = do
    (next, token) <- consume
    case next of
        [DataType name] -> do
            (next, token) <- consume
            case next of
                [LeftParen] -> do
                    constructorFields <- fields
                    return $ Just $ Constructor name constructorFields
                _ -> do
                    reportError "Expecting '(' to define fields after constructor name in type constructor declaration" token
                    return Nothing
        _ -> do
            reportError "Expecting type constructor declaration" token
            return Nothing

fields :: State ParserState [Field]
fields = peek >>= \case
    [RightParen] -> do
        advance 1
        return []
    _ -> recurseFields

recurseFields :: State ParserState [Field]
recurseFields = do
    (next, token) <- consume
    case next of
        [Token.Identifier identifier] -> do
            (next, token) <- consume
            case next of
                [Colon] -> do
                    maybeAnnotation <- typeAnnotation
                    case maybeAnnotation of
                        Just annotation -> do
                            (next, token) <- consume
                            case next of
                                [Comma] -> do
                                    otherFields <- recurseFields
                                    return $ Field identifier annotation : otherFields
                                [RightParen] -> return [Field identifier annotation]
                                _ -> do
                                    reportError "Expecting ',' or ')' after field in type definition" token
                                    return []
                        Nothing -> return []
                _ -> do
                    reportError "Fields must be followed by a type annotation" token
                    return []
        _ -> do
            reportError "Expecting identifier in field list" token
            return []

bindings :: State ParserState [Binding]
bindings = peek >>= \case
    [Token.Identifier identifier] -> do
        advance 1
        peek >>= \case
            [Equals] -> do
                advance 1
                constant identifier
            _ -> function identifier
    _ -> return []

function :: String -> State ParserState [Binding]
function identifier = do
    generics <- genericParams
    (next, token) <- consume
    case next of
        [LeftParen] -> do
            params <- parameters
            (next, token) <- consume
            case next of
                [Colon] -> do
                    maybeAnnotation <- typeAnnotation
                    case maybeAnnotation of
                        Just annotation -> do
                            (next, token) <- consume
                            case next of
                                [Equals] -> do
                                    expr <- expression
                                    let binding = Binding identifier (Literal $ Function [] generics params annotation expr) TUndefined
                                    otherBindings <- bindings
                                    return $ binding : otherBindings
                                _ -> do
                                    reportError "Expecting '=' after return type in function definition" token
                                    return []
                        Nothing -> return []
                _ -> do
                    reportError "Expecting annotation of return type after parameters in function definition" token
                    return []
        _ -> do
            let error = case generics of
                    [] -> "Expecting '=', '<' or '(' after identifier in binding"
                    _ -> "Expecting '(' after generic parameters in function definition"
            reportError error token
            return []

parameters :: State ParserState [(String, TypeAnnotation)]
parameters = peek >>= \case
    [RightParen] -> do
        advance 1
        return []
    _ -> recurseParameters

recurseParameters :: State ParserState [(String, TypeAnnotation)]
recurseParameters = do
    (next, token) <- consume
    case next of
        [Token.Identifier identifier] -> do
            (next, token) <- consume
            case next of
                [Colon] -> do
                    maybeAnnotation <- typeAnnotation
                    case maybeAnnotation of
                        Just annotation -> do
                            (next, token) <- consume
                            case next of
                                [Comma] -> do
                                    params <- recurseParameters
                                    return $ (identifier, annotation) : params
                                [RightParen] -> return [(identifier, annotation)]
                                _ -> do
                                    reportError "Expecting ',' or ')' after parameter in function definition" token
                                    return []
                        Nothing -> return []
                _ -> do
                    reportError "Function parameters must be followed by a type annotation" token
                    return []
        _ -> do
            reportError "Expecting identifier in parameter list" token
            return []

typeAnnotation :: State ParserState (Maybe TypeAnnotation)
typeAnnotation = do
    (next, token) <- consume
    case next of
        [Token.Identifier name] -> return $ Just $ ConstantAnnotation name []
        [DataType name] -> do
            genericArguments <- genericArgs
            return $ Just $ ConstantAnnotation name genericArguments
        [LeftParen] -> functionType
        _ -> do
            reportError "Invalid type annotation" token
            return Nothing

genericArgs :: State ParserState [TypeAnnotation]
genericArgs = peek >>= \case
    [LeftAngle] -> do
        advance 1
        recurseGenericArgs
    _ -> return []

recurseGenericArgs :: State ParserState [TypeAnnotation]
recurseGenericArgs = do
    maybeAnnotation <- typeAnnotation
    case maybeAnnotation of
        Just annotation -> do
            (next, token) <- consume
            case next of
                [Comma] -> do
                    otherArgs <- recurseGenericArgs
                    return $ annotation : otherArgs
                [RightAngle] -> return [annotation]
                _ -> do
                    reportError "Expecting ',' or '>' after generic argument in type annotation" token
                    return [annotation]
        Nothing -> return []

functionType :: State ParserState (Maybe TypeAnnotation)
functionType = do
    params <- parameterTypes
    returnType <- typeAnnotation
    return $ FunctionAnnotation [] params <$> returnType

parameterTypes :: State ParserState [TypeAnnotation]
parameterTypes = peek >>= \case
    [RightParen] -> do
        advance 1
        return []
    _ -> recurseParameterTypes

recurseParameterTypes :: State ParserState [TypeAnnotation]
recurseParameterTypes = do
    maybeAnnotation <- typeAnnotation
    case maybeAnnotation of
        Just annotation -> do
            (next, token) <- consume
            case next of
                [Comma] -> do
                    params <- recurseParameterTypes
                    return $ annotation : params
                [RightParen] -> return [annotation]
                _ -> do
                    reportError "Expecting ',' or ')' after parameter type in type annotation" token
                    return []
        Nothing -> return []

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
    [Dot] -> do
        advance 1
        (next, token) <- consume
        case next of
            [Token.Identifier identifier] ->
                recurseCall $ FieldAccess callee identifier
            _ -> do
                reportError "Expecting field identifier after '.'" token
                return None
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
        [Token.Case] -> do
            variable <- expression
            (next, token) <- consume
            if next == [Of] then do
                cases <- caseList
                return $ CaseOf variable cases
            else do
                reportError "Expecting 'of' after variable in 'case' expression" token
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
        [DataType identifier] -> return $ AST.Identifier identifier $ Token.position $ head token
        _ -> do
            reportError "Unexpected token" token
            return None

caseList :: State ParserState [Case]
caseList = peek >>= \case
    [DataType name] -> do
        advance 1
        (next, token) <- consume
        if next == [Arrow] then do
            expr <- expression
            otherCases <- caseList
            return $ AST.Case name expr : otherCases
        else do
            reportError "Expecting '->' after constructor name in 'case' expression" token
            return []
    _ -> return []

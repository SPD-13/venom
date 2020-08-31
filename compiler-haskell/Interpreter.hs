module Interpreter where

import Data.List (intercalate)
import Control.Monad (unless)
import Control.Monad.ST
import Data.STRef

import Operator
import AST
import qualified Environment as E

interpret :: AST -> String
interpret (Bindings bindings) =
    runST $ interpretBindings bindings

getEnvValue (Binding identifier value _) = (identifier, E.Expression value)
getIdentifier (Binding identifier _ _) = identifier

interpretBindings :: [Binding] -> ST s String
interpretBindings bindings = do
    env <- E.new
    sequence_ $ map (E.set env . getEnvValue) bindings
    let identifiers = map getIdentifier bindings
    values <- sequence $ map (interpretIdentifier [] env) identifiers
    return $ intercalate "\n" $ map (\(i, v) -> i ++ " = " ++ show v) $ zip identifiers values

interpretIdentifier :: [String] -> E.Env s -> String -> ST s (E.Computed s)
interpretIdentifier evaluating env identifier = do
    val <- E.get identifier env
    case val of
        Just ref -> do
            value <- readSTRef ref
            case value of
                E.Expression expr -> do
                    computed <- interpretExpression (identifier:evaluating) env expr
                    writeSTRef ref $ E.Computed computed
                    return computed
                E.Computed computed -> return computed
        Nothing -> return E.RuntimeError

interpretExpression :: [String] -> E.Env s -> Expression -> ST s (E.Computed s)
interpretExpression evaluating env expression =
    let eval = interpretExpression evaluating
    in case expression of
        Let bindings expr -> do
            sequence_ $ map (E.set env . getEnvValue) bindings
            result <- eval env expr
            sequence_ $ map (E.delete env . getIdentifier) bindings
            return result
        If condition trueValue falseValue -> do
            result <- eval env condition
            if isTrue $ result then
                eval env trueValue
            else
                eval env falseValue
        Binary left op right -> do
            leftValue <- eval env left
            rightValue <- eval env right
            case op of
                Plus -> case (leftValue, rightValue) of
                    (E.Integer l, E.Integer r) -> return $ E.Integer $ l + r
                    _ -> return E.RuntimeError
                Minus -> case (leftValue, rightValue) of
                    (E.Integer l, E.Integer r) -> return $ E.Integer $ l - r
                    _ -> return E.RuntimeError
                Times -> case (leftValue, rightValue) of
                    (E.Integer l, E.Integer r) -> return $ E.Integer $ l * r
                    _ -> return E.RuntimeError
                Equality -> return $ E.Bool $ leftValue == rightValue
                Inequality -> return $ E.Bool $ leftValue /= rightValue
                And -> return $ E.Bool $ isTrue leftValue && isTrue rightValue
                Or -> return $ E.Bool $ isTrue leftValue || isTrue rightValue
                _ -> return E.RuntimeError
        Call callee arguments -> do
            concreteCallee <- eval env callee
            case concreteCallee of
                E.Closure closureEnv (Function params expr) -> do
                    concreteArguments <- sequence $ map (eval env) arguments
                    functionEnv <- E.copy closureEnv
                    sequence_ $ map (E.set functionEnv) $ zip params $ map E.Computed concreteArguments
                    interpretExpression [] functionEnv expr
                _ -> return E.RuntimeError
        Literal literal ->
            case literal of
                Integer integer -> return $ E.Integer integer
                Bool bool -> return $ E.Bool bool
                Lambda freeVars function -> do
                    closureEnv <- E.new
                    sequence_ $ map (resolveFreeVariable evaluating env closureEnv) freeVars
                    return $ E.Closure closureEnv function
        Identifier identifier -> interpretIdentifier evaluating env identifier

resolveFreeVariable :: [String] -> E.Env s -> E.Env s -> String -> ST s ()
resolveFreeVariable evaluating currentEnv closureEnv identifier = do
    val <- E.get identifier currentEnv
    case val of
        Just ref -> do
            E.setRef closureEnv identifier ref
            value <- readSTRef ref
            case value of
                E.Expression expr ->
                    unless (elem identifier evaluating) $ do
                        computed <- interpretExpression (identifier:evaluating) currentEnv expr
                        writeSTRef ref $ E.Computed computed
                computed -> return ()
        Nothing -> return ()

isTrue = (== E.Bool True)

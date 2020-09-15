module TypeChecker where

import Control.Monad
import Control.Monad.ST
import Data.STRef

import AST
import Error
import Operator
import TypeEnvironment

typeCheck :: AST -> Either [Error] AST
typeCheck ast = runST $ checkBindings ast

checkBindings :: AST -> ST s (Either [Error] AST)
checkBindings (Bindings bindings) = do
    env <- new
    errors <- newSTRef []
    sequence_ $ map (set env . getEnvValue) bindings
    sequence_ $ map (inferIdentifier env errors) $ map getIdentifier bindings
    finalErrors <- readSTRef errors
    if null finalErrors then do
        typedBindings <- sequence $ map (getBinding env) $ map getIdentifier bindings
        return $ Right $ Bindings typedBindings
    else
        return $ Left finalErrors

inferIdentifier :: Env s -> STRef s [Error] -> String -> ST s ExpressionType
inferIdentifier env errors identifier = do
    val <- get identifier env
    let dummy = TUndefined
    case val of
        Just ref -> do
            value <- readSTRef ref
            case value of
                Expression expr -> do
                    (typedExpr, exprType) <- inferExpression env errors expr
                    writeSTRef ref $ Typed typedExpr exprType
                    return exprType
                Typed _ exprType -> return exprType
        Nothing -> return dummy

getEnvValue (Binding identifier value _) = (identifier, Expression value)
getIdentifier (Binding identifier _ _) = identifier

getBinding :: Env s -> String -> ST s Binding
getBinding env identifier = do
    val <- get identifier env
    let dummy = Binding "" None TUndefined
    case val of
        Just ref -> do
            value <- readSTRef ref
            case value of
                Typed expr eType -> return $ Binding identifier expr eType
                Expression _ -> return dummy
        Nothing -> return dummy

reportError :: STRef s [Error] -> Error -> ST s ()
reportError errors error = modifySTRef errors (error:)

inferExpression :: Env s -> STRef s [Error] -> Expression -> ST s (Expression, ExpressionType)
inferExpression env errors expression =
    let ie = inferExpression env errors
        err = reportError errors
        unimplemented = (expression, TUndefined)
    in case expression of
        Let bindings expr -> do
            sequence_ $ map (set env . getEnvValue) bindings
            sequence_ $ map (inferIdentifier env errors) $ map getIdentifier bindings
            typedBindings <- sequence $ map (getBinding env) $ map getIdentifier bindings
            (typedExpr, exprType) <- ie expr
            return (Let typedBindings typedExpr, exprType)
        Literal literal -> case literal of
            AST.Integer _ -> return (expression, TInteger)
            _ -> return unimplemented
        Binary left op right -> do
            (typedLeft, leftType) <- ie left
            (typedRight, rightType) <- ie right
            case op of
                Plus -> do
                    when (leftType /= TInteger) $ err $ Error "First argument of '+' must be an integer" EOF
                    when (rightType /= TInteger) $ err $ Error "Second argument of '+' must be an integer" EOF
                    return (Binary typedLeft op typedRight, TInteger)
                _ -> return unimplemented
        _ -> return unimplemented

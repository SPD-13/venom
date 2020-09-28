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
            typedBindings <- sequence $ map (getBinding env . getIdentifier) bindings
            (typedExpr, exprType) <- ie expr
            sequence_ $ map (delete env . getIdentifier) bindings
            return (Let typedBindings typedExpr, exprType)
        If condition true false -> do
            (typedCondition, conditionType) <- ie condition
            when (conditionType /= TBool) $ err $ Error "Condition of 'if' must be a boolean" EOF
            (typedTrue, trueType) <- ie true
            (typedFalse, falseType) <- ie false
            when (trueType /= falseType) $ err $ Error "Both branches of 'if' must have the same type" EOF
            return (If typedCondition typedTrue typedFalse, trueType)
        Literal literal -> case literal of
            AST.Integer _ -> return (expression, TInteger)
            AST.Bool _ -> return (expression, TBool)
            AST.Char _ -> return (expression, TChar)
            AST.String _ -> return (expression, TString)
            Lambda freeVars (Function params returnType expr) -> do
                let paramToEnv (identifier, annotation) = (identifier, Typed None annotation)
                sequence_ $ map (set env . paramToEnv) params
                (typedExpr, exprType) <- ie expr
                when (returnType /= exprType) $ err $ Error "Function body does not match the annotated return type" EOF
                sequence_ $ map (delete env . fst) params
                return (Literal (Lambda freeVars (Function params returnType typedExpr)), TFunction (map snd params) returnType)
        Binary left op right -> do
            (typedLeft, leftType) <- ie left
            (typedRight, rightType) <- ie right
            case op of
                Plus -> do
                    when (leftType /= TInteger) $ err $ Error "First argument of '+' must be an integer" EOF
                    when (rightType /= TInteger) $ err $ Error "Second argument of '+' must be an integer" EOF
                    return (Binary typedLeft op typedRight, TInteger)
                _ -> return unimplemented
        Call callee arguments -> do
            (typedCallee, calleeType) <- ie callee
            case calleeType of
                TFunction parameterTypes functionType -> do
                    typedArguments <- sequence $ map ie arguments
                    checkArguments errors (map snd typedArguments) parameterTypes
                    return (Call typedCallee (map fst typedArguments), functionType)
                _ -> do
                    err $ Error "Callee must be a function" EOF
                    return (Call typedCallee arguments, TUndefined)
        Identifier identifier _ -> do
            identifierType <- inferIdentifier env errors identifier
            return (expression, identifierType)
        None -> return (None, TUndefined)

checkArguments :: STRef s [Error] -> [ExpressionType] -> [ExpressionType] -> ST s ()
checkArguments errors arguments parameters = do
    let err = reportError errors
        lenArgs = length arguments
        lenParams = length parameters
    when (lenArgs /= lenParams) $ err $ Error ("Expected " ++ show lenParams ++ " arguments but got " ++ show lenArgs) EOF
    sequence_ $ map (checkArgument errors) $ zip [1..] $ zip arguments parameters

checkArgument :: STRef s [Error] -> (Integer, (ExpressionType, ExpressionType)) -> ST s ()
checkArgument errors (index, (argType, paramType)) =
    when (argType /= paramType) $ reportError errors $ Error ("Argument " ++ show index ++ " should be '" ++ show paramType ++ "' but got '" ++ show argType ++ "'") EOF

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
    sequence_ $ map (checkIdentifier env errors . getIdentifier) bindings
    finalErrors <- readSTRef errors
    if null finalErrors then do
        typedBindings <- sequence $ map (getBinding env . getIdentifier) bindings
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
                Expression expr Nothing -> do
                    (typedExpr, exprType) <- inferExpression env errors expr
                    writeSTRef ref $ Typed typedExpr exprType
                    return exprType
                Expression _ (Just annotation) -> return annotation
                Typed _ exprType -> return exprType
        Nothing -> return dummy

checkIdentifier :: Env s -> STRef s [Error] -> String -> ST s ()
checkIdentifier env errors identifier = do
    val <- get identifier env
    let dummy = ()
    case val of
        Just ref -> do
            value <- readSTRef ref
            case value of
                Expression expr maybeAnnotation -> do
                    (typedExpr, exprType) <- inferExpression env errors expr
                    case maybeAnnotation of
                        Nothing -> writeSTRef ref $ Typed typedExpr exprType
                        Just annotation -> do
                            when (annotation /= exprType) $ reportError errors $ Error ("Type annotation does not match inferred type\nGot: " ++ show exprType) EOF
                            writeSTRef ref $ Typed typedExpr annotation
                Typed _ _ -> return ()
        Nothing -> return dummy

getEnvValue :: Binding -> (String, Value)
getEnvValue (Binding identifier value _) = case value of
    Literal (Lambda _ (Function params returnType _)) -> (identifier, Expression value (Just (TFunction (map snd params) returnType)))
    _ -> (identifier, Expression value Nothing)

getIdentifier :: Binding -> String
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
                Expression _ _ -> return dummy
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
            sequence_ $ map (checkIdentifier env errors . getIdentifier) bindings
            typedBindings <- sequence $ map (getBinding env . getIdentifier) bindings
            (typedExpr, exprType) <- ie expr
            sequence_ $ map (delete env . getIdentifier) bindings
            return (Let typedBindings typedExpr, exprType)
        If condition true false -> do
            (typedCondition, conditionType) <- ie condition
            when (conditionType /= TBool) $ err $ Error ("Condition of 'if' must be a boolean\nGot: " ++ show conditionType) EOF
            (typedTrue, trueType) <- ie true
            (typedFalse, falseType) <- ie false
            when (trueType /= falseType) $ err $ Error ("Both branches of 'if' must have the same type\nTrue type: " ++ show trueType ++ "\nFalse type: " ++ show falseType) EOF
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
                when (returnType /= exprType) $ err $ Error ("Function body does not match the annotated return type\nGot: " ++ show exprType) EOF
                sequence_ $ map (delete env . fst) params
                return (Literal (Lambda freeVars (Function params returnType typedExpr)), TFunction (map snd params) returnType)
        Binary left op right -> do
            (typedLeft, leftType) <- ie left
            (typedRight, rightType) <- ie right
            case op of
                Plus -> do
                    when (leftType /= TInteger) $ err $ Error ("First argument of '+' must be an integer\nGot: " ++ show leftType) EOF
                    when (rightType /= TInteger) $ err $ Error ("Second argument of '+' must be an integer\nGot: " ++ show rightType) EOF
                    return (Binary typedLeft op typedRight, TInteger)
                Minus -> do
                    when (leftType /= TInteger) $ err $ Error ("First argument of '-' must be an integer\nGot: " ++ show leftType) EOF
                    when (rightType /= TInteger) $ err $ Error ("Second argument of '-' must be an integer\nGot: " ++ show rightType) EOF
                    return (Binary typedLeft op typedRight, TInteger)
                Equality -> do
                    when (leftType /= rightType) $ err $ Error ("Both arguments of '==' must be the same type") EOF
                    return (Binary typedLeft op typedRight, TBool)
                _ -> return unimplemented
        Call callee arguments -> do
            (typedCallee, calleeType) <- ie callee
            case calleeType of
                TFunction parameterTypes functionType -> do
                    typedArguments <- sequence $ map ie arguments
                    checkArguments errors (map snd typedArguments) parameterTypes
                    return (Call typedCallee (map fst typedArguments), functionType)
                _ -> do
                    err $ Error ("Callee must be a function\nGot: " ++ show calleeType) EOF
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

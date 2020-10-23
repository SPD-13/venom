module TypeChecker where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
import Data.Maybe
import Data.List (foldl')

import AST
import Error
import Operator
import Environment

typeCheck :: AST -> Either [Error] AST
typeCheck ast = runST $ checkBindings ast

checkBindings :: AST -> ST s (Either [Error] AST)
checkBindings (AST declaredTypes bindings) = do
    let getTypeName (TypeDeclaration name _) = name
        types = foldl' declareType M.empty $ map getTypeName declaredTypes
    env <- new
    errors <- newSTRef []
    sequence_ $ map (inferConstructors types env errors) declaredTypes
    sequence_ $ map (set env . getEnvValue types errors) bindings
    typedBindings <- sequence $ map (checkIdentifier types env errors . getIdentifier) bindings
    finalErrors <- readSTRef errors
    if null finalErrors then
        return $ Right $ AST declaredTypes typedBindings
    else
        return $ Left finalErrors

declareType :: M.Map String ExpressionType -> String -> M.Map String ExpressionType
declareType types typeName = M.insert typeName (TCustom typeName Nothing) types

inferConstructors :: M.Map String ExpressionType -> TypeEnv s -> STRef s [Error] -> TypeDeclaration -> ST s ()
inferConstructors types env errors (TypeDeclaration typeName constructors) =
    sequence_ $ fmap (inferConstructor types env errors typeName) constructors

inferConstructor :: M.Map String ExpressionType -> TypeEnv s -> STRef s [Error] -> String -> Constructor -> ST s ()
inferConstructor types env errors typeName (Constructor name fields) =
    let getAnnotation (Field _ annotation) = annotation
        fieldAnnotations = map getAnnotation fields
        constructorAnnotation = FunctionAnnotation fieldAnnotations (ConstantAnnotation typeName)
        constructorType = fromMaybe TUndefined $ annotationToType types errors constructorAnnotation
    in set env (name, Typed None constructorType)

inferIdentifier :: M.Map String ExpressionType -> TypeEnv s -> STRef s [Error] -> String -> ST s ExpressionType
inferIdentifier types env errors identifier = do
    val <- get identifier env
    let dummy = TUndefined
    case val of
        Just ref -> do
            value <- readSTRef ref
            case value of
                Untyped expr Nothing -> do
                    (typedExpr, exprType) <- inferExpression types env errors expr
                    writeSTRef ref $ Typed typedExpr exprType
                    return exprType
                Untyped _ (Just annotation) -> return annotation
                Typed _ exprType -> return exprType
        Nothing -> return dummy

checkIdentifier :: M.Map String ExpressionType -> TypeEnv s -> STRef s [Error] -> String -> ST s Binding
checkIdentifier types env errors identifier = do
    val <- get identifier env
    let dummy = Binding "" None TUndefined
    case val of
        Just ref -> do
            value <- readSTRef ref
            case value of
                Untyped expr maybeAnnotation -> do
                    (typedExpr, exprType) <- inferExpression types env errors expr
                    let annotation = fromMaybe exprType maybeAnnotation
                    when (annotation /= exprType) $ reportError errors $ Error ("Type annotation does not match inferred type\nGot: " ++ show exprType) EOF
                    writeSTRef ref $ Typed typedExpr annotation
                    return $ Binding identifier typedExpr annotation
                Typed expr eType -> return $ Binding identifier expr eType
        Nothing -> return dummy

annotationToType :: M.Map String ExpressionType -> STRef s [Error] -> TypeAnnotation -> Maybe ExpressionType
annotationToType types errors annotation =
    let recurse = annotationToType types errors in case annotation of
    ConstantAnnotation name -> case name of
        "Int" -> Just TInteger
        "Bool" -> Just TBool
        "Char" -> Just TChar
        "String" -> Just TString
        _ -> M.lookup name types
    FunctionAnnotation params returnType ->
        let maybeParams = sequence $ map recurse params
            maybeReturnType = recurse returnType
        in TFunction <$> maybeParams <*> maybeReturnType

getEnvValue :: M.Map String ExpressionType -> STRef s [Error] -> Binding -> (String, TypeValue)
getEnvValue types errors (Binding identifier value _) =
    let
        annotation = case value of
            Literal (Lambda _ (Function params returnType _)) ->
                annotationToType types errors $ FunctionAnnotation (map snd params) returnType
            _ -> Nothing
    in (identifier, Untyped value annotation)

getIdentifier :: Binding -> String
getIdentifier (Binding identifier _ _) = identifier

reportError :: STRef s [Error] -> Error -> ST s ()
reportError errors error = modifySTRef errors (error:)

inferExpression :: M.Map String ExpressionType -> TypeEnv s -> STRef s [Error] -> Expression -> ST s (Expression, ExpressionType)
inferExpression types env errors expression =
    let ie = inferExpression types env errors
        err = reportError errors
        unimplemented = (expression, TUndefined)
    in case expression of
        Let bindings expr -> do
            sequence_ $ map (set env . getEnvValue types errors) bindings
            typedBindings <- sequence $ map (checkIdentifier types env errors . getIdentifier) bindings
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
        FieldAccess record field -> do
            (typedRecord, recordType) <- ie record
            case recordType of
                TCustom _ maybeFieldTypes -> case maybeFieldTypes of
                    Just fieldTypes ->
                        let maybeResult = M.lookup field fieldTypes
                        in case maybeResult of
                            Just result -> return (FieldAccess typedRecord field, result)
                            Nothing -> do
                                err $ Error ("Field '" ++ field ++ "' does not exist on the specified record") EOF
                                return (FieldAccess typedRecord field, TUndefined)
                    Nothing -> do
                        err $ Error "Can't access field on object with multiple constructors\nUse 'case' statement to split on constructors" EOF
                        return (FieldAccess typedRecord field, TUndefined)
                _ -> do
                    err $ Error ("Left side of field access must be a record\nGot: " ++ show recordType) EOF
                    return (FieldAccess typedRecord field, TUndefined)
        Literal literal -> case literal of
            AST.Integer _ -> return (expression, TInteger)
            AST.Bool _ -> return (expression, TBool)
            AST.Char _ -> return (expression, TChar)
            AST.String _ -> return (expression, TString)
            Lambda freeVars (Function params returnAnnotation expr) ->
                let maybeFunctionType = annotationToType types errors $ FunctionAnnotation (map snd params) returnAnnotation
                in case maybeFunctionType of
                    Just functionType@(TFunction paramTypes returnType) -> do
                        let paramToEnv (identifier, paramType) = (identifier, Typed None paramType)
                        sequence_ $ map (set env . paramToEnv) $ zip (map fst params) paramTypes
                        (typedExpr, exprType) <- ie expr
                        when (returnType /= exprType) $ err $ Error ("Function body does not match the annotated return type\nGot: " ++ show exprType) EOF
                        sequence_ $ map (delete env . fst) params
                        return (Literal (Lambda freeVars (Function params returnAnnotation typedExpr)), functionType)
                    _ -> return (expression, TUndefined)
        Identifier identifier _ -> do
            identifierType <- inferIdentifier types env errors identifier
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

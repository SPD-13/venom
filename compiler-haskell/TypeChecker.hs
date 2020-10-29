module TypeChecker where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
import Data.Maybe
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B

import AST
import Error
import Operator
import Environment

type HashTable s k v = B.HashTable s k v
type TypeDeclarations s = HashTable s String DeclarationType
data DeclarationType
    = Declared TypeDeclaration
    | Evaluated ExpressionType

typeCheck :: AST -> Either [Error] AST
typeCheck ast = runST $ checkBindings ast

checkBindings :: AST -> ST s (Either [Error] AST)
checkBindings (AST declaredTypes bindings) = do
    types <- H.new
    let declare typeDeclaration@(TypeDeclaration name _) = H.insert types name $ Declared typeDeclaration
    sequence_ $ map declare declaredTypes
    env <- new
    errors <- newSTRef []
    sequence_ $ map (inferConstructors types env errors) declaredTypes
    sequence_ $ map (set env <=< getEnvValue types errors) bindings
    typedBindings <- sequence $ map (checkIdentifier types env errors . getIdentifier) bindings
    finalErrors <- readSTRef errors
    if null finalErrors then
        return $ Right $ AST declaredTypes typedBindings
    else
        return $ Left $ reverse finalErrors

inferConstructors :: TypeDeclarations s -> TypeEnv s -> STRef s [Error] -> TypeDeclaration -> ST s ()
inferConstructors types env errors (TypeDeclaration typeName constructors) =
    sequence_ $ fmap (inferConstructor types env errors typeName) constructors

inferConstructor :: TypeDeclarations s -> TypeEnv s -> STRef s [Error] -> String -> Constructor -> ST s ()
inferConstructor types env errors typeName constructor@(Constructor name _) = do
    maybeFieldTypes <- getFieldTypes types errors constructor
    let constructorType = case maybeFieldTypes of
            Just (paramTypes, fieldTypes) -> TFunction paramTypes $ TCustom typeName $ Just fieldTypes
            Nothing -> TUndefined
    set env (name, Typed None constructorType)

getFieldTypes :: TypeDeclarations s -> STRef s [Error] -> Constructor -> ST s (Maybe ([ExpressionType], FieldTypes))
getFieldTypes types errors (Constructor _ fields) = do
    let getName (Field fieldName _) = fieldName
        getAnnotation (Field _ annotation) = annotation
        makeResult fieldTypes = (fieldTypes, M.fromList $ zip (map getName fields) fieldTypes)
    maybeFieldTypes <- sequence $ map (annotationToType types errors . getAnnotation) fields
    return $ fmap makeResult $ sequence maybeFieldTypes

inferIdentifier :: TypeDeclarations s -> TypeEnv s -> STRef s [Error] -> String -> ST s ExpressionType
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

checkIdentifier :: TypeDeclarations s -> TypeEnv s -> STRef s [Error] -> String -> ST s Binding
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

getDeclarationType :: TypeDeclarations s -> STRef s [Error] -> String -> ST s (Maybe ExpressionType)
getDeclarationType types errors name = do
    maybeDeclarationType <- H.lookup types name
    case maybeDeclarationType of
        Just declarationType -> case declarationType of
            Declared (TypeDeclaration _ constructors) -> do
                expressionType <- case constructors of
                    constructor :| [] -> do
                        maybeFieldTypes <- getFieldTypes types errors constructor
                        case maybeFieldTypes of
                            Just (_, fieldTypes) -> return $ TCustom name $ Just fieldTypes
                            Nothing -> return TUndefined
                    _ -> return $ TCustom name Nothing
                H.insert types name $ Evaluated expressionType
                return $ Just expressionType
            Evaluated expressionType -> return $ Just expressionType
        Nothing -> return Nothing

annotationToType :: TypeDeclarations s -> STRef s [Error] -> TypeAnnotation -> ST s (Maybe ExpressionType)
annotationToType types errors annotation =
    let recurse = annotationToType types errors in case annotation of
    ConstantAnnotation name -> case name of
        "Int" -> return $ Just TInteger
        "Bool" -> return $ Just TBool
        "Char" -> return $ Just TChar
        "String" -> return $ Just TString
        _ -> getDeclarationType types errors name
    FunctionAnnotation params returnType -> do
        maybeParams <- sequence $ map recurse params
        maybeReturnType <- recurse returnType
        return $ TFunction <$> sequence maybeParams <*> maybeReturnType

getEnvValue :: TypeDeclarations s -> STRef s [Error] -> Binding -> ST s (String, TypeValue)
getEnvValue types errors (Binding identifier value _) = do
    annotation <- case value of
        Literal (Lambda _ (Function params returnType _)) ->
            annotationToType types errors $ FunctionAnnotation (map snd params) returnType
        _ -> return Nothing
    return (identifier, Untyped value annotation)

getIdentifier :: Binding -> String
getIdentifier (Binding identifier _ _) = identifier

reportError :: STRef s [Error] -> Error -> ST s ()
reportError errors error = modifySTRef errors (error:)

inferExpression :: TypeDeclarations s -> TypeEnv s -> STRef s [Error] -> Expression -> ST s (Expression, ExpressionType)
inferExpression types env errors expression =
    let ie = inferExpression types env errors
        err = reportError errors
        unimplemented = (expression, TUndefined)
    in case expression of
        Let bindings expr -> do
            sequence_ $ map (set env <=< getEnvValue types errors) bindings
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
            Lambda freeVars (Function params returnAnnotation expr) -> do
                maybeFunctionType <- annotationToType types errors $ FunctionAnnotation (map snd params) returnAnnotation
                case maybeFunctionType of
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
    when (not $ isSubtype argType paramType) $ reportError errors $ Error ("Argument " ++ show index ++ " should be '" ++ show paramType ++ "' but got '" ++ show argType ++ "'") EOF

isSubtype :: ExpressionType -> ExpressionType -> Bool
isSubtype argument parameter = case (argument, parameter) of
    (TCustom argName _, TCustom paramName _) -> argName == paramName
    _ -> argument == parameter

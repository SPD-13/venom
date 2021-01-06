module TypeChecker where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty((:|)), toList)
import qualified Data.Set as S

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B

import AST
import Error
import Operator

type HashTable s k v = B.HashTable s k v

-- Used to resolve type annotations
type Types s = HashTable s String ExpressionType

-- Used to resolve the type of identifiers
data TypeValue
    = Untyped Expression
    | Typed Expression ExpressionType
type TypeEnv s = HashTable s String TypeValue

-- Used to unify generic parameters
data ParameterType
    = Unset
    | Alias String
    | Set ExpressionType
type TypeArgs s = HashTable s String ParameterType

set env = uncurry $ H.insert env

typeCheck :: AST -> Either [Error] AST
typeCheck ast = runST $ checkBindings ast

checkBindings :: AST -> ST s (Either [Error] AST)
checkBindings (AST declaredTypes bindings) = do
    types <- H.new
    env <- H.new
    errors <- newSTRef []
    let declare (TypeDeclaration typeName typeParams constructors) = do
        let insertParam param = H.insert types param $ TParameter param
            getName (AST.Constructor name _) = name
            typeInfo = TypeInfo typeName typeParams $ toList $ fmap getName constructors
            insertType maybeFieldTypes = H.insert types typeName $ TCustom typeInfo maybeFieldTypes
            getTypes = getFieldTypes types errors
        mapM_ insertParam typeParams
        case constructors of
            (AST.Constructor name fields) :| [] -> do
                result@(_, fieldTypes) <- getTypes fields
                inferConstructor result env typeInfo name
                insertType $ Just fieldTypes
            _ -> do
                insertType Nothing
                let infer (AST.Constructor name fields) = do
                    result <- getTypes fields
                    inferConstructor result env typeInfo name
                mapM_ infer constructors
        mapM_ (H.delete types) typeParams
    mapM_ declare declaredTypes
    mapM_ (set env . getEnvValue) bindings
    typedBindings <- mapM (checkIdentifier types env errors . getIdentifier) bindings
    finalErrors <- readSTRef errors
    if null finalErrors then
        return $ Right $ AST declaredTypes typedBindings
    else
        return $ Left $ reverse finalErrors

inferConstructor :: ([ExpressionType], FieldTypes) -> TypeEnv s -> TypeInfo -> String -> ST s ()
inferConstructor (paramTypes, fieldTypes) env typeInfo name = do
    let constructorType = TFunction [] paramTypes $ TCustom typeInfo $ Just fieldTypes
    H.insert env name $ Typed None constructorType

getFieldTypes :: Types s -> STRef s [Error] -> [Field] -> ST s ([ExpressionType], FieldTypes)
getFieldTypes types errors fields = do
    let getName (Field fieldName _) = fieldName
        getAnnotation (Field _ annotation) = annotation
    fieldTypes <- mapM (annotationToType types errors . getAnnotation) fields
    return (fieldTypes, M.fromList $ zip (map getName fields) fieldTypes)

checkIdentifier :: Types s -> TypeEnv s -> STRef s [Error] -> String -> ST s Binding
checkIdentifier types env errors identifier = do
    val <- H.lookup env identifier
    let dummy = Binding "" None TUndefined
    case val of
        Just value -> case value of
            Untyped expr -> do
                (typedExpr, exprType) <- inferExpression types env errors identifier expr
                H.insert env identifier $ Typed typedExpr exprType
                return $ Binding identifier typedExpr exprType
            Typed expr eType -> return $ Binding identifier expr eType
        Nothing -> return dummy

getDeclarationType :: Types s -> STRef s [Error] -> String -> ST s ExpressionType
getDeclarationType types errors name = do
    maybeDeclarationType <- H.lookup types name
    case maybeDeclarationType of
        Just exprType -> return exprType
        Nothing -> do
            reportError errors $ Error ("Invalid type annotation: '" ++ name ++ "'\nType does not exist") EOF
            return TUndefined

annotationToType :: Types s -> STRef s [Error] -> TypeAnnotation -> ST s ExpressionType
annotationToType types errors annotation =
    let recurse = annotationToType types errors in case annotation of
    ConstantAnnotation name _ -> case name of
        "Int" -> return TInteger
        "Bool" -> return TBool
        "Char" -> return TChar
        "String" -> return TString
        _ -> getDeclarationType types errors name
    FunctionAnnotation genericParams paramTypeAnnotations returnTypeAnnotation -> do
        paramTypes <- mapM recurse paramTypeAnnotations
        returnType <- recurse returnTypeAnnotation
        return $ TFunction genericParams paramTypes returnType

getEnvValue :: Binding -> (String, TypeValue)
getEnvValue (Binding identifier value _) = (identifier, Untyped value)

getIdentifier :: Binding -> String
getIdentifier (Binding identifier _ _) = identifier

reportError :: STRef s [Error] -> Error -> ST s ()
reportError errors error = modifySTRef' errors (error:)

inferExpression :: Types s -> TypeEnv s -> STRef s [Error] -> String -> Expression -> ST s (Expression, ExpressionType)
inferExpression types env errors evaluating expression =
    let ie = inferExpression types env errors evaluating
        err = reportError errors
        dummy = (expression, TUndefined)
        unimplemented = dummy
    in case expression of
        Let bindings expr -> do
            mapM_ (set env . getEnvValue) bindings
            typedBindings <- mapM (checkIdentifier types env errors . getIdentifier) bindings
            (typedExpr, exprType) <- ie expr
            mapM_ (H.delete env . getIdentifier) bindings
            return (Let typedBindings typedExpr, exprType)
        If condition true false -> do
            (typedCondition, conditionType) <- ie condition
            when (conditionType /= TBool) $ err $ Error ("Condition of 'if' must be a boolean\nGot: " ++ show conditionType) EOF
            (typedTrue, trueType) <- ie true
            (typedFalse, falseType) <- ie false
            unless (isSameType trueType falseType) $ err $ Error ("Both branches of 'if' must have the same type\nTrue type: " ++ show trueType ++ "\nFalse type: " ++ show falseType) EOF
            return (If typedCondition typedTrue typedFalse, trueType)
        CaseOf variable cases -> do
            (typedVariable, variableType) <- ie variable
            let errorValue = (CaseOf typedVariable cases, TUndefined)
            case variableType of
                TCustom (TypeInfo typeName _ constructorNames) Nothing -> do
                    let getName (Case name _) = name
                        caseNames = map getName cases
                        checkName name =
                            when (name `notElem` caseNames) $ err $ Error ("Missing constructor '" ++ name ++ "' in 'case' expression") EOF
                    mapM_ checkName constructorNames
                    let checkCase (typedCases, returnType) (Case name expr) = do
                        when (name `notElem` constructorNames) $ err $ Error ("'" ++ name ++ "' is not a valid constructor for the variable of type '" ++ typeName ++ "'") EOF
                        (typedExpr, exprType) <- case variable of
                            -- When the variable in a case expression is a single identifier, specialize its type while evaluating the case subexpressions so we can access its fields
                            Identifier identifier _ -> do
                                let dummy = (None, TUndefined)
                                -- TODO: Extract identifier lookup out of 'checkCase'
                                val <- H.lookup env identifier
                                case val of
                                    Just value -> case value of
                                        Typed typedBinding _ -> do
                                            constructorVal <- H.lookup env name
                                            case constructorVal of
                                                Just constructorValue -> case constructorValue of
                                                    Typed _ (TFunction _ _ constructorType) -> do
                                                        H.insert env identifier $ Typed typedBinding constructorType
                                                        result <- ie expr
                                                        H.insert env identifier $ Typed typedBinding variableType
                                                        return result
                                                    _ -> return dummy
                                                Nothing -> return (expr, TUndefined)
                                        _ -> return dummy
                                    Nothing -> return dummy
                            _ -> ie expr
                        let returnValue = Case name typedExpr : typedCases
                        case returnType of
                            TUndefined -> return (returnValue, exprType)
                            _ -> do
                                let errorMessage = "All branches of 'case' expression must return the same type\nExpected: " ++ show returnType ++ "\nGot: " ++ show exprType
                                unless (isSameType exprType returnType) $ err $ Error errorMessage EOF
                                return (returnValue, returnType)
                    (typedCases, returnType) <- foldM checkCase ([], TUndefined) cases
                    return (CaseOf typedVariable $ reverse typedCases, returnType)
                TCustom _ _ -> do
                    err $ Error "Cannot use 'case' expression on variable with only one possible constructor" EOF
                    return errorValue
                _ -> do
                    err $ Error ("Cannot use 'case' expression on built-in type\nGot: " ++ show variableType) EOF
                    return errorValue
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
                    when (leftType /= rightType) $ err $ Error "Both arguments of '==' must be the same type" EOF
                    return (Binary typedLeft op typedRight, TBool)
                _ -> return unimplemented
        Call callee arguments -> do
            (typedCallee, calleeType) <- ie callee
            case calleeType of
                TFunction genericParams parameterTypes returnType -> do
                    typedArguments <- mapM ie arguments
                    typeParams <- H.fromList $ zip genericParams $ repeat Unset
                    checkArguments typeParams errors (map snd typedArguments) parameterTypes
                    resolvedReturnType <- resolveReturnType typeParams returnType
                    return (Call typedCallee (map fst typedArguments), resolvedReturnType)
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
                        err $ Error "Can't access field on object with multiple constructors\nUse 'case' expression to split on constructors" EOF
                        return (FieldAccess typedRecord field, TUndefined)
                _ -> do
                    err $ Error ("Left side of field access must be a record\nGot: " ++ show recordType) EOF
                    return (FieldAccess typedRecord field, TUndefined)
        Literal literal -> case literal of
            AST.Integer _ -> return (expression, TInteger)
            AST.Bool _ -> return (expression, TBool)
            AST.Char _ -> return (expression, TChar)
            AST.String _ -> return (expression, TString)
            AST.Function freeVars genericParams params returnAnnotation expr -> do
                let insertParam param = H.insert types param $ TParameter param
                mapM_ insertParam genericParams
                functionType <- annotationToType types errors $ FunctionAnnotation genericParams (map snd params) returnAnnotation
                case functionType of
                    TFunction _ paramTypes returnType -> do
                        H.insert env evaluating $ Typed None functionType
                        let paramToEnv ((identifier, _), paramType) = (identifier, Typed None paramType)
                        mapM_ (set env . paramToEnv) $ zip params paramTypes
                        (typedExpr, exprType) <- ie expr
                        let error = Error ("Function body does not match the annotated return type\nGot: " ++ show exprType ++ "\nExpected: " ++ show returnType) EOF
                        unless (isSameType exprType returnType) $ err error
                        mapM_ (H.delete env . fst) params
                        mapM_ (H.delete types) genericParams
                        return (Literal $ AST.Function freeVars genericParams params returnAnnotation typedExpr, functionType)
                    _ -> return dummy
        Identifier identifier _ -> do
            (Binding _ _ identifierType) <- checkIdentifier types env errors identifier
            return (expression, identifierType)
        None -> return (None, TUndefined)

resolveReturnType :: TypeArgs s -> ExpressionType -> ST s ExpressionType
resolveReturnType typeParams returnType = case returnType of
    TParameter genericParam -> do
        lookupResult <- H.lookup typeParams genericParam
        case lookupResult of
            Just Unset -> return TUndefined
            Just (Alias alias) -> resolveReturnType typeParams $ TParameter alias
            Just (Set exprType) -> return exprType
            Nothing -> return returnType
    TFunction _ paramTypes rType -> do
        genericParamSet <- newSTRef S.empty
        resolvedParamTypes <- mapM (resolveFunctionPart typeParams genericParamSet) paramTypes
        resolvedReturnType <- resolveFunctionPart typeParams genericParamSet rType
        genericParams <- readSTRef genericParamSet
        return $ TFunction (S.toList genericParams) resolvedParamTypes resolvedReturnType
    _ -> return returnType

resolveFunctionPart :: TypeArgs s -> STRef s (S.Set String) -> ExpressionType -> ST s ExpressionType
resolveFunctionPart typeParams genericParams part = case part of
    TParameter genericParam -> do
        lookupResult <- H.lookup typeParams genericParam
        case lookupResult of
            Just Unset -> do
                modifySTRef' genericParams $ S.insert genericParam
                return part
            Just (Alias alias) -> resolveFunctionPart typeParams genericParams $ TParameter alias
            Just (Set exprType) -> return exprType
            Nothing -> return part -- TODO: Add this to set of non-generic params and then rename generic params if there are conflicts
    TFunction _ paramTypes rType -> do
        resolvedParamTypes <- mapM (resolveFunctionPart typeParams genericParams) paramTypes
        resolvedReturnType <- resolveFunctionPart typeParams genericParams rType
        return $ TFunction [] resolvedParamTypes resolvedReturnType
    _ -> return part

checkArguments :: TypeArgs s -> STRef s [Error] -> [ExpressionType] -> [ExpressionType] -> ST s ()
checkArguments typeParams errors arguments parameters = do
    let lenArgs = length arguments
        lenParams = length parameters
    when (lenArgs /= lenParams) $ reportError errors $ Error ("Expected " ++ show lenParams ++ " arguments but got " ++ show lenArgs) EOF
    genericArgs <- H.new
    zipWithM_ (unifyFunctionPart typeParams genericArgs errors) parameters arguments

unifyFunctionPart :: TypeArgs s -> TypeArgs s -> STRef s [Error] -> ExpressionType -> ExpressionType -> ST s ()
unifyFunctionPart typeParams argTypeParams errors paramType argType = case paramType of
    TParameter genericParam -> do
        lookupResult <- H.lookup typeParams genericParam
        case lookupResult of
            -- Actual generic parameter from the current function call
            Just Unset ->
                case argType of
                    TParameter argGenericParam -> do
                        lookupResult <- H.lookup argTypeParams argGenericParam
                        case lookupResult of
                            -- Actual generic parameter from the argument function
                            Just Unset -> H.insert argTypeParams argGenericParam $ Alias genericParam
                            Just (Alias alias) -> aliasParam typeParams genericParam alias
                            Just (Set exprType) -> H.insert typeParams genericParam $ Set exprType
                            -- Generic parameter from the enclosing scope, same as concrete type
                            Nothing -> H.insert typeParams genericParam $ Set argType
                    -- Concrete type
                    _ -> H.insert typeParams genericParam $ Set argType
            Just (Alias alias) -> unifyFunctionPart typeParams argTypeParams errors (TParameter alias) argType
            Just (Set exprType) -> unifyWithSetParam typeParams argTypeParams errors exprType argType
            -- Generic parameter from the enclosing scope, same as concrete type
            Nothing -> unifyWithSetParam typeParams argTypeParams errors paramType argType
    TFunction _ paramTypes returnType -> case argType of
        TFunction genericParams argParamTypes argReturnType ->
            if length paramTypes == length argParamTypes then do
                genericArgs <- if null genericParams
                    then return argTypeParams
                    else H.fromList $ zip genericParams $ repeat Unset
                zipWithM_ (unifyFunctionPart typeParams genericArgs errors) paramTypes argParamTypes
                unifyFunctionPart typeParams genericArgs errors returnType argReturnType
            else reportError errors $ Error "" EOF
        _ -> reportError errors $ Error "" EOF
    -- Concrete type
    _ -> unifyWithSetParam typeParams argTypeParams errors paramType argType

aliasParam :: TypeArgs s -> String -> String -> ST s ()
aliasParam typeParams base target =
    if base == target then
        return ()
    else do
        lookupResult <- H.lookup typeParams target
        case lookupResult of
            Just (Alias alias) -> aliasParam typeParams base alias
            _ -> H.insert typeParams base $ Alias target

unifyWithSetParam :: TypeArgs s -> TypeArgs s -> STRef s [Error] -> ExpressionType -> ExpressionType -> ST s ()
unifyWithSetParam typeParams argTypeParams errors paramType argType = do
    let check otherType = return ()
    case argType of
        TParameter argGenericParam -> do
            lookupResult <- H.lookup argTypeParams argGenericParam
            case lookupResult of
                -- Actual generic parameter from the argument function
                Just Unset -> H.insert argTypeParams argGenericParam $ Set paramType
                Just (Alias alias) -> unifyFunctionPart typeParams argTypeParams errors (TParameter alias) paramType
                Just (Set exprType) -> check exprType
                -- Generic parameter from the enclosing scope, same as concrete type
                Nothing -> check argType
        -- Concrete type
        _ -> check argType

isSameType :: ExpressionType -> ExpressionType -> Bool
isSameType aType bType = case (aType, bType) of
    (TCustom (TypeInfo aTypeName _ _) _, TCustom (TypeInfo bTypeName _ _) _) -> aTypeName == bTypeName
    _ -> aType == bType

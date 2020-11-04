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
    let typeInfo = TypeInfo typeName $ getConstructorNames constructors
    in sequence_ $ fmap (inferConstructor types env errors typeInfo) constructors

getConstructorNames :: NonEmpty Constructor -> [String]
getConstructorNames (head :| tail) =
    let getName (AST.Constructor name _) = name
    in getName head : map getName tail

inferConstructor :: TypeDeclarations s -> TypeEnv s -> STRef s [Error] -> TypeInfo -> Constructor -> ST s ()
inferConstructor types env errors typeInfo constructor@(AST.Constructor name _) = do
    (paramTypes, fieldTypes) <- getFieldTypes True types errors constructor
    let constructorType = TFunction paramTypes $ TCustom typeInfo $ Just fieldTypes
    set env (name, Typed None constructorType)

getFieldTypes :: Bool -> TypeDeclarations s -> STRef s [Error] -> Constructor -> ST s ([ExpressionType], FieldTypes)
getFieldTypes report types errors (AST.Constructor _ fields) = do
    let getName (Field fieldName _) = fieldName
        getAnnotation (Field _ annotation) = annotation
    fieldTypes <- sequence $ map (annotationToType report types errors . getAnnotation) fields
    return (fieldTypes, M.fromList $ zip (map getName fields) fieldTypes)

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

getDeclarationType :: Bool -> TypeDeclarations s -> STRef s [Error] -> String -> ST s ExpressionType
getDeclarationType report types errors name = do
    maybeDeclarationType <- H.lookup types name
    case maybeDeclarationType of
        Just declarationType -> case declarationType of
            Declared (TypeDeclaration _ constructors) -> do
                let typeInfo = TypeInfo name $ getConstructorNames constructors
                expressionType <- case constructors of
                    constructor :| [] -> do
                        (_, fieldTypes) <- getFieldTypes report types errors constructor
                        return $ TCustom typeInfo $ Just fieldTypes
                    _ -> return $ TCustom typeInfo Nothing
                H.insert types name $ Evaluated expressionType
                return expressionType
            Evaluated expressionType -> return expressionType
        Nothing -> do
            when report $ reportError errors $ Error ("Invalid type annotation: '" ++ name ++ "'\nType does not exist") EOF
            return TUndefined

annotationToType :: Bool -> TypeDeclarations s -> STRef s [Error] -> TypeAnnotation -> ST s ExpressionType
annotationToType report types errors annotation =
    let recurse = annotationToType report types errors in case annotation of
    ConstantAnnotation name -> case name of
        "Int" -> return TInteger
        "Bool" -> return TBool
        "Char" -> return TChar
        "String" -> return TString
        _ -> getDeclarationType report types errors name
    FunctionAnnotation paramTypeAnnotations returnTypeAnnotation -> do
        paramTypes <- sequence $ map recurse paramTypeAnnotations
        returnType <- recurse returnTypeAnnotation
        return $ TFunction paramTypes returnType

getEnvValue :: TypeDeclarations s -> STRef s [Error] -> Binding -> ST s (String, TypeValue)
getEnvValue types errors (Binding identifier value _) = do
    annotation <- case value of
        Literal (Lambda _ (AST.Function params returnType _)) ->
            fmap Just $ annotationToType False types errors $ FunctionAnnotation (map snd params) returnType
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
            when (not $ isSameType trueType falseType) $ err $ Error ("Both branches of 'if' must have the same type\nTrue type: " ++ show trueType ++ "\nFalse type: " ++ show falseType) EOF
            return (If typedCondition typedTrue typedFalse, trueType)
        CaseOf variable cases -> do
            (typedVariable, variableType) <- ie variable
            let errorValue = (CaseOf typedVariable cases, TUndefined)
            case variableType of
                TCustom (TypeInfo typeName constructorNames) Nothing -> do
                    let getName (Case name _) = name
                        caseNames = map getName cases
                        checkName name =
                            when (name `notElem` caseNames) $ err $ Error ("Missing constructor '" ++ name ++ "' in 'case' expression") EOF
                    sequence_ $ map checkName constructorNames
                    let checkCase (typedCases, returnType) (Case name expr) = do
                        when (name `notElem` constructorNames) $ err $ Error ("'" ++ name ++ "' is not a valid constructor for the variable of type '" ++ typeName ++ "'") EOF
                        (typedExpr, exprType) <- case variable of
                            -- When the variable in a case expression is a single identifier, specialize its type while evaluating the case subexpressions so we can access its fields
                            Identifier identifier _ -> do
                                let dummy = (None, TUndefined)
                                -- TODO: Extract identifier lookup out of 'checkCase'
                                val <- get identifier env
                                case val of
                                    Just ref -> do
                                        value <- readSTRef ref
                                        case value of
                                            Typed typedBinding _ -> do
                                                constructorVal <- get name env
                                                case constructorVal of
                                                    Just constructorRef -> do
                                                        constructorValue <- readSTRef constructorRef
                                                        case constructorValue of
                                                            Typed _ (TFunction _ constructorType) -> do
                                                                writeSTRef ref $ Typed typedBinding constructorType
                                                                result <- ie expr
                                                                writeSTRef ref $ Typed typedBinding variableType
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
                                when (not $ isSameType exprType returnType) $ err $ Error errorMessage EOF
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
            Lambda freeVars (AST.Function params returnAnnotation expr) -> do
                functionType <- annotationToType False types errors $ FunctionAnnotation (map snd params) returnAnnotation
                case functionType of
                    TFunction paramTypes returnType -> do
                        let paramToEnv (identifier, paramType) = (identifier, Typed None paramType)
                        sequence_ $ map (set env . paramToEnv) $ zip (map fst params) paramTypes
                        (typedExpr, exprType) <- ie expr
                        when (not $ isSameType returnType exprType) $ err $ Error ("Function body does not match the annotated return type\nGot: " ++ show exprType) EOF
                        sequence_ $ map (delete env . fst) params
                        return (Literal (Lambda freeVars (AST.Function params returnAnnotation typedExpr)), functionType)
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
    when (not $ isSameType argType paramType) $ reportError errors $ Error ("Argument " ++ show index ++ " should be '" ++ show paramType ++ "' but got '" ++ show argType ++ "'") EOF

isSameType :: ExpressionType -> ExpressionType -> Bool
isSameType aType bType = case (aType, bType) of
    (TCustom (TypeInfo aTypeName _) _, TCustom (TypeInfo bTypeName _) _) -> aTypeName == bTypeName
    _ -> aType == bType

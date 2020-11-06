module Interpreter where

import Data.List (find, intercalate)
import qualified Data.Map as M
import Control.Monad (unless)
import Control.Monad.ST
import Data.STRef

import Operator
import AST
import qualified Environment as E

interpret :: AST -> String
interpret ast = runST $ interpretAST ast

getEnvValue (Binding identifier value _) = (identifier, E.Expression value)
getIdentifier (Binding identifier _ _) = identifier

interpretAST :: AST -> ST s String
interpretAST (AST types bindings) = do
    env <- E.new
    sequence_ $ map (registerConstructors env) types
    sequence_ $ map (E.set env . getEnvValue) bindings
    values <- sequence $ map (interpretIdentifier [] env . getIdentifier) bindings
    return $ intercalate "\n" $ map (\(Binding i _ t, v) -> i ++ " : " ++ show t ++ " = " ++ show v) $ zip bindings values

registerConstructors :: E.Env s -> TypeDeclaration -> ST s ()
registerConstructors env (TypeDeclaration _ constructors) =
    let getFieldName (Field fieldName _) = fieldName
        registerConstructor (Constructor name fields) =
            let constructor = E.Constructor name $ map getFieldName fields
            in E.set env (name, E.Computed constructor)
    in sequence_ $ fmap registerConstructor constructors

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
    let eval = interpretExpression evaluating env
    in case expression of
        Let bindings expr -> do
            sequence_ $ map (E.set env . getEnvValue) bindings
            result <- eval expr
            sequence_ $ map (E.delete env . getIdentifier) bindings
            return result
        If condition trueValue falseValue -> do
            result <- eval condition
            if isTrue $ result then
                eval trueValue
            else
                eval falseValue
        CaseOf variable cases -> do
            concreteVariable <- eval variable
            case concreteVariable of
                E.Custom constructor _ -> do
                    let matchCase (Case name _) = name == constructor
                        maybeCase = find matchCase cases
                    case maybeCase of
                        Just (Case _ expr) -> eval expr
                        Nothing -> return E.RuntimeError
                _ -> return E.RuntimeError
        Binary left op right -> do
            leftValue <- eval left
            rightValue <- eval right
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
            concreteCallee <- eval callee
            case concreteCallee of
                E.Closure closureEnv (E.Function params expr) -> do
                    concreteArguments <- sequence $ map eval arguments
                    functionEnv <- E.copy closureEnv
                    sequence_ $ map (E.set functionEnv) $ zip params $ map E.Computed concreteArguments
                    interpretExpression [] functionEnv expr
                E.Constructor name fieldNames -> do
                    concreteArguments <- sequence $ map eval arguments
                    let fields = M.fromList $ zip fieldNames concreteArguments
                    return $ E.Custom name fields
                _ -> return E.RuntimeError
        FieldAccess record field -> do
            concreteRecord <- eval record
            case concreteRecord of
                E.Custom _ fields ->
                    let maybeValue = M.lookup field fields
                    in case maybeValue of
                        Just value -> return value
                        Nothing -> return E.RuntimeError
                _ -> return E.RuntimeError
        Literal literal -> case literal of
            Integer integer -> return $ E.Integer integer
            Bool bool -> return $ E.Bool bool
            Char char -> return $ E.Char char
            String string -> return $ E.String string
            Lambda freeVars (Function params _ expr) -> do
                closureEnv <- E.new
                sequence_ $ map (resolveFreeVariable evaluating env closureEnv) freeVars
                return $ E.Closure closureEnv $ E.Function (map fst params) expr
        Identifier identifier _ -> interpretIdentifier evaluating env identifier
        None -> return E.RuntimeError

resolveFreeVariable :: [String] -> E.Env s -> E.Env s -> String -> ST s ()
resolveFreeVariable evaluating currentEnv closureEnv identifier = do
    val <- E.get identifier currentEnv
    case val of
        Just ref -> do
            E.setRef closureEnv identifier ref
            -- TODO: Is evaluating the variable at this point necessary?
            value <- readSTRef ref
            case value of
                E.Expression expr ->
                    unless (elem identifier evaluating) $ do
                        computed <- interpretExpression (identifier:evaluating) currentEnv expr
                        writeSTRef ref $ E.Computed computed
                E.Computed _ -> return ()
        Nothing -> return ()

isTrue = (== E.Bool True)

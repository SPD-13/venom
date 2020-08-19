module Interpreter where

import Data.List (foldl')

import Operator
import AST
import qualified Environment as E

interpret :: AST -> String
interpret (Bindings bindings) =
    show $ interpretBindings E.new bindings

interpretBindings :: E.Env -> [Binding] -> E.Env
interpretBindings env bindings =
    foldl' interpretBinding env bindings

interpretBinding :: E.Env -> Binding -> E.Env
interpretBinding env (Binding identifier expr _) =
    E.set env (identifier, E.Computed (interpretExpression env expr))

interpretExpression :: E.Env -> Expression -> (E.Env, E.Computed)
interpretExpression env expression =
    case expression of
        Let bindings expr ->
            let localEnv = interpretBindings env bindings
            in interpretExpression localEnv expr
        If condition trueValue falseValue ->
            let (newEnv, result) = interpretExpression env condition
            in
                if isTrue $ result then
                    interpretExpression newEnv trueValue
                else
                    interpretExpression newEnv falseValue
        Binary left op right ->
            let (newEnv, leftValue) = interpretExpression env left
                (newestEnv, rightValue) = interpretExpression newEnv right
            in
                case op of
                    Plus -> case (leftValue, rightValue) of
                        (E.Integer l, E.Integer r) -> (newestEnv, E.Integer $ l + r)
                        _ -> (newestEnv, E.RuntimeError)
                    Minus -> case (leftValue, rightValue) of
                        (E.Integer l, E.Integer r) -> (newestEnv, E.Integer $ l - r)
                        _ -> (newestEnv, E.RuntimeError)
                    Times -> case (leftValue, rightValue) of
                        (E.Integer l, E.Integer r) -> (newestEnv, E.Integer $ l * r)
                        _ -> (newestEnv, E.RuntimeError)
                    Equality -> (newestEnv, E.Bool $ leftValue == rightValue)
                    Inequality -> (newestEnv, E.Bool $ leftValue /= rightValue)
                    And -> (newestEnv, E.Bool $ isTrue leftValue && isTrue rightValue)
                    Or -> (newestEnv, E.Bool $ isTrue leftValue || isTrue rightValue)
                    _ -> (newestEnv, E.RuntimeError)
        Call callee arguments ->
            let (newEnv, concreteCallee) = interpretExpression env callee
            in case concreteCallee of
                E.Closure closureEnv (Function params expr) ->
                    let concreteArguments = map (interpretExpression env) arguments
                        functionEnv = foldl' E.set closureEnv $ zip params concreteArguments
                    in interpretExpression functionEnv expr
                _ -> (newEnv, E.RuntimeError)
        Literal literal ->
            case literal of
                Integer integer -> (env, E.Integer integer)
                Bool bool -> (env, E.Bool bool)
                Lambda freeVars function ->
                    let (newEnv, closureEnv) = foldl' resolveFreeVariable (env, E.new) freeVars
                    in (newEnv, E.Closure closureEnv function)
        Identifier identifier ->
            case E.get identifier env of
                Just value -> case value of
                    E.Expression expr ->
                        let (newEnv, computed) = interpretExpression env expr
                        in (E.set newEnv (identifier, E.Computed computed), computed)
                    E.Computed computed -> (env, computed)
                Nothing -> (env, E.RuntimeError)

resolveFreeVariable :: (E.Env, E.Env) -> String -> (E.Env, E.Env)
resolveFreeVariable (currentEnv, closureEnv) identifier =
    case E.get identifier currentEnv of
        Just value -> case value of
            E.Expression expr ->
                let (newEnv, computed) = interpretExpression currentEnv expr
                    val = E.Computed computed
                in (E.set newEnv (identifier, val), E.set closureEnv (identifier, val))
            computed -> (currentEnv, E.set closureEnv (identifier, computed))
        Nothing -> (currentEnv, closureEnv)

isTrue = (== E.Bool True)

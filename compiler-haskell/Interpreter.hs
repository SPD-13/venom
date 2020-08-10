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
    E.set env (identifier, interpretExpression env expr)

interpretExpression :: E.Env -> Expression -> E.Literal
interpretExpression env expression =
    case expression of
        Let bindings expr ->
            let localEnv = interpretBindings env bindings
            in interpretExpression localEnv expr
        If condition trueValue falseValue ->
            if isTrue $ interpretExpression env condition then
                interpretExpression env trueValue
            else
                interpretExpression env falseValue
        Binary left op right ->
            let leftValue = interpretExpression env left
                rightValue = interpretExpression env right
            in
                case op of
                    Plus -> case (leftValue, rightValue) of
                        (E.Integer l, E.Integer r) -> E.Integer $ l + r
                        _ -> E.RuntimeError
                    Minus -> case (leftValue, rightValue) of
                        (E.Integer l, E.Integer r) -> E.Integer $ l - r
                        _ -> E.RuntimeError
                    Times -> case (leftValue, rightValue) of
                        (E.Integer l, E.Integer r) -> E.Integer $ l * r
                        _ -> E.RuntimeError
                    Equality -> E.Bool $ leftValue == rightValue
                    Inequality -> E.Bool $ leftValue /= rightValue
                    And -> E.Bool $ isTrue leftValue && isTrue rightValue
                    Or -> E.Bool $ isTrue leftValue || isTrue rightValue
                    _ -> E.RuntimeError
        Call callee arguments ->
            let concreteCallee = interpretExpression env callee
            in case concreteCallee of
                E.Function closure params expr ->
                    let concreteArguments = map (interpretExpression env) arguments
                        functionEnv = foldl' E.set closure $ zip params concreteArguments
                    in interpretExpression functionEnv expr
                _ -> E.RuntimeError
        Literal literal ->
            case literal of
                Integer integer -> E.Integer integer
                Bool bool -> E.Bool bool
                Function params expr -> E.Function env params expr
        Identifier identifier ->
            case E.get identifier env of
                Just value -> value
                Nothing -> E.RuntimeError

isTrue = (== E.Bool True)

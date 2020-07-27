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
interpretBinding env binding =
    let
        (identifier, value) =
            case binding of
                (FunctionDeclaration identifier params expr) ->
                    (identifier, Function params expr)
                (Constant identifier expr) ->
                    (identifier, interpretExpression env expr)
    in E.set env (identifier, value)

interpretExpression :: E.Env -> Expression -> Concrete
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
                        (Integer l, Integer r) -> Integer $ l + r
                    Minus -> case (leftValue, rightValue) of
                        (Integer l, Integer r) -> Integer $ l - r
                    Times -> case (leftValue, rightValue) of
                        (Integer l, Integer r) -> Integer $ l * r
                    Equality -> Bool $ leftValue == rightValue
                    Inequality -> Bool $ leftValue /= rightValue
                    And -> Bool $ isTrue leftValue && isTrue rightValue
                    Or -> Bool $ isTrue leftValue || isTrue rightValue
                    _ -> Integer 0
        Call callee arguments ->
            let concreteCallee = interpretExpression env callee
            in case concreteCallee of
                Function params expr ->
                    let concreteArguments = map (interpretExpression env) arguments
                        functionEnv = foldl' E.set env $ zip params concreteArguments
                    in interpretExpression functionEnv expr
                _ -> Integer 0
        Literal literal -> literal
        Identifier identifier ->
            case E.get identifier env of
                Just value -> value
                Nothing -> Integer 0

isTrue = (== Bool True)

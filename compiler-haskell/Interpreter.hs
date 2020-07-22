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
interpretBinding env (Binding identifier expr) =
    let value = interpretExpression expr env
    in E.set identifier value env

interpretExpression :: Expression -> E.Env -> Concrete
interpretExpression expression env =
    case expression of
        Let bindings expr ->
            let localEnv = interpretBindings env bindings
            in interpretExpression expr localEnv
        If condition trueValue falseValue ->
            if interpretExpression condition env == Bool True then
                interpretExpression trueValue env
            else
                interpretExpression falseValue env
        Binary left op right ->
            let
                leftValue = interpretExpression left env
                rightValue = interpretExpression right env
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
                    _ -> Integer 0
        Literal literal -> literal
        Identifier identifier ->
            case E.get identifier env of
                Just value -> value
                Nothing -> Integer 0

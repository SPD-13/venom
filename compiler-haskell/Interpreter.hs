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

interpretExpression :: Expression -> E.Env -> Integer
interpretExpression expression env =
    case expression of
        Let bindings expr ->
            let localEnv = interpretBindings env bindings
            in interpretExpression expr localEnv
        Binary left op right ->
            let
                leftValue = interpretExpression left env
                rightValue = interpretExpression right env
            in
                case op of
                    Plus -> leftValue + rightValue
                    Minus -> leftValue - rightValue
                    Times -> leftValue * rightValue
                    _ -> 0
        Integer integer -> integer
        Value identifier ->
            case E.get identifier env of
                Just value -> value
                Nothing -> 0
        _ -> 0

module Interpreter where

import Data.List (foldl', intercalate)
import qualified Data.Map as M

import Operator
import AST

type Env = M.Map String Integer

interpret :: AST -> String
interpret (Bindings bindings) =
    printEnv $ foldl' interpretBinding M.empty bindings

interpretBinding :: Env -> Binding -> Env
interpretBinding env (Binding identifier expr) =
    let value = interpretExpression expr env
    in M.insert identifier value env

interpretExpression :: Expression -> Env -> Integer
interpretExpression expression env =
    case expression of
        Let bindings expr ->
            0
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
            case M.lookup identifier env of
                Just value -> value
                Nothing -> 0
        _ -> 0

printEnv :: Env -> String
printEnv env =
    intercalate "\n" $ map (\(k, v) -> k ++ " = " ++ show v) $ M.toList env

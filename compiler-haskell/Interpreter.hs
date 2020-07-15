module Interpreter where

import Operator
import AST

interpret :: AST -> Integer
interpret (Expression expression) =
    interpretExpression expression

interpretExpression :: Expression -> Integer
interpretExpression expression =
    case expression of
        Binary left op right ->
            let
                leftValue = interpretExpression left
                rightValue = interpretExpression right
            in
                case op of
                    Plus -> leftValue + rightValue
                    Minus -> leftValue - rightValue
                    _ -> 0
        Integer integer -> integer
        None -> 0


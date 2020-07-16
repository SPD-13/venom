module Interpreter where

import Operator
import AST

interpret :: AST -> Integer
interpret (Bindings bindings) =
    last $ map interpretBinding bindings

interpretBinding :: Binding -> Integer
interpretBinding (Binding _ expr) = interpretExpression expr

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

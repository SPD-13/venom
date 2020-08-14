module TypeChecker where

import AST
import Operator
import Token

data TypeError = TypeError String TokenPosition

typeCheck :: AST -> AST
typeCheck (Bindings bindings) = Bindings $ checkBindings bindings

checkBindings :: [Binding] -> [Binding]
checkBindings bindings =
    map checkBinding bindings

checkBinding :: Binding -> Binding
checkBinding (Binding identifier expr _) =
    Binding identifier expr (infer expr)

infer :: Expression -> ExpressionType
infer expression = case expression of
    Literal literal -> case literal of
        AST.Integer _ -> TInteger
        _ -> TUndefined
    Binary left op right -> case op of
        Plus -> TInteger
        _ -> TUndefined
    _ -> TUndefined

check :: Expression -> ExpressionType -> Maybe TypeError
check expression expectedType =
    if infer expression == expectedType then
        Nothing
    else
        Nothing

findFreeVariables :: [String] -> Expression -> [String]
findFreeVariables params expression = case expression of
    _ -> []

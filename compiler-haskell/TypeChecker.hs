module TypeChecker where

import AST

check :: AST -> AST
check (Bindings bindings) = Bindings $ checkBindings bindings

checkBindings :: [Binding] -> [Binding]
checkBindings bindings =
    map checkBinding bindings

checkBinding :: Binding -> Binding
checkBinding (Binding identifier expr _) =
    Binding identifier expr (infer expr)

infer :: Expression -> BindingType
infer expression = case expression of
    _ -> TUndefined

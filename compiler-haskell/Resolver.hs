module Resolver where

import AST

resolve :: AST -> AST
resolve (Bindings bindings) =
    Bindings $ map resolveBinding bindings

resolveBinding :: Binding -> Binding
resolveBinding (Binding identifier expression _) =
    undefined

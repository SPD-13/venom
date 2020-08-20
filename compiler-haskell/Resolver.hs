module Resolver where

import Data.List (nub)

import AST

resolve :: AST -> AST
resolve (Bindings bindings) =
    let (resolvedBindings, bindingVars) = unzip $ map resolveBinding bindings
        vars = concat bindingVars
        undefinedErrors = filter (`notElem` (map getIdentifier bindings)) (nub vars)
    in Bindings resolvedBindings

resolveBinding :: Binding -> (Binding, [String])
resolveBinding (Binding identifier expression expressionType) =
    let (resolvedExpression, vars) = resolveExpression expression
    in (Binding identifier resolvedExpression expressionType, vars)

getIdentifier (Binding identifier _ _) = identifier

resolveExpression :: Expression -> (Expression, [String])
resolveExpression expression = case expression of
    Let bindings expr ->
        let (resolvedBindings, bindingVars) = unzip $ map resolveBinding bindings
            (resolvedExpression, expressionVars) = resolveExpression expr
            vars = expressionVars ++ (concat bindingVars)
            freeVars = filter (`notElem` (map getIdentifier bindings)) (nub vars)
        in (Let resolvedBindings resolvedExpression, freeVars)
    If condition true false ->
        let (resolvedCondition, vars1) = resolveExpression condition
            (resolvedTrue, vars2) = resolveExpression true
            (resolvedFalse, vars3) = resolveExpression false
        in (If resolvedCondition resolvedTrue resolvedFalse, vars1 ++ vars2 ++ vars3)
    Binary left op right ->
        let (resolvedLeft, vars1) = resolveExpression left
            (resolvedRight, vars2) = resolveExpression right
        in (Binary resolvedLeft op resolvedRight, vars1 ++ vars2)
    Call callee args ->
        let (resolvedCallee, vars) = resolveExpression callee
            (resolvedArgs, argsVars) = unzip $ map resolveExpression args
        in (Call resolvedCallee resolvedArgs, vars ++ (concat argsVars))
    Literal literal -> case literal of
        Lambda _ (Function params expr) ->
            let (resolvedExpression, vars) = resolveExpression expr
                freeVars = filter (`notElem` params) (nub vars)
            in (Literal (Lambda freeVars (Function params resolvedExpression)), freeVars)
        _ -> (expression, [])
    Identifier identifier -> (expression, [identifier])
    None -> (None, [])

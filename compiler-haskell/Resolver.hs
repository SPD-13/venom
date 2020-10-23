module Resolver where

import Data.List.NonEmpty (NonEmpty((:|)))

import AST
import qualified Error as E
import qualified Position as P

data Variable = Variable
    { name :: String
    , position :: P.Position
    }

resolve :: AST -> Either [E.Error] AST
resolve (AST types bindings) =
    let (resolvedBindings, bindingVars) = unzip $ map resolveBinding bindings
        vars = concat bindingVars
        typeConstructors = concatMap getConstructors types
        bindingNames = map getIdentifier bindings
        declaredNames = typeConstructors ++ bindingNames
        undefinedErrors = filter ((`notElem` declaredNames) . name) vars
        reportError var = E.Error ("Unknown variable '" ++ name var ++ "'") (E.Position $ position var)
    in if null undefinedErrors then
        Right $ AST types resolvedBindings
    else
        Left $ map reportError undefinedErrors

getConstructors :: TypeDeclaration -> [String]
getConstructors (TypeDeclaration _ (cons :| otherCons)) =
    let getName (Constructor name _) = name
    in getName cons : map getName otherCons

resolveBinding :: Binding -> (Binding, [Variable])
resolveBinding (Binding identifier expression expressionType) =
    let (resolvedExpression, vars) = resolveExpression expression
    in (Binding identifier resolvedExpression expressionType, vars)

getIdentifier (Binding identifier _ _) = identifier

resolveExpression :: Expression -> (Expression, [Variable])
resolveExpression expression = case expression of
    Let bindings expr ->
        let (resolvedBindings, bindingVars) = unzip $ map resolveBinding bindings
            (resolvedExpression, expressionVars) = resolveExpression expr
            vars = expressionVars ++ (concat bindingVars)
            bindingNames = map getIdentifier bindings
            freeVars = filter ((`notElem` bindingNames) . name) vars
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
    FieldAccess record field ->
        let (resolvedRecord, vars) = resolveExpression record
        in (FieldAccess resolvedRecord field, vars)
    Literal literal -> case literal of
        Lambda _ (Function params returnType expr) ->
            let (resolvedExpression, vars) = resolveExpression expr
                freeVars = filter ((`notElem` (map fst params)) . name) vars
                freeNames = map name freeVars
            in (Literal (Lambda freeNames (Function params returnType resolvedExpression)), freeVars)
        _ -> (expression, [])
    Identifier identifier position -> (expression, [Variable identifier position])
    None -> (None, [])

module Transpiler where

import Data.List (foldl', intercalate)

import AST
import Operator
import Position

var = "let "
tab = "  "

transpile :: AST -> String
transpile (AST types bindings) =
    let constructors = concatMap outputConstructors types
        app = outputExpression 0 $ Let bindings $ Identifier "main" $ Position 0 0
        output = "console.log(" ++ app ++ ")\n"
    in constructors ++ output

outputConstructors :: TypeDeclaration -> String
outputConstructors (TypeDeclaration _ constructors) =
    concatMap outputConstructor constructors

outputConstructor :: Constructor -> String
outputConstructor (Constructor name fields) =
    let getName (Field fieldName _) = fieldName
        fieldNames = map (toJS . getName) fields
        params = intercalate ", " fieldNames
        outputSetter fieldName = "\n" ++ tab ++ "this." ++ fieldName ++ " = " ++ fieldName
        setters = concatMap outputSetter fieldNames
        constructor = "function " ++ name ++ "(" ++ params ++ ") {" ++ setters ++ "\n}\n"
        new = var ++ toJS name ++ " = (...args) => new " ++ name ++ "(...args)\n"
    in constructor ++ new

outputExpression :: Int -> Expression -> String
outputExpression tabLevel expression =
    let output = outputExpression tabLevel
        outputIndented = outputExpression $ tabLevel + 1
        base = concat $ replicate tabLevel tab
        indent = concat $ replicate (tabLevel + 1) tab
    in case expression of
        Let bindings expr ->
            let outputSetter (Binding name value _) = "\n" ++ indent ++ var ++ toJS name ++ " = " ++ output value
                setters = concatMap outputSetter bindings
                result = "\n" ++ indent ++ "return " ++ output expr
            in "(() => {" ++ setters ++ result ++ "\n" ++ base ++ "})()"
        If condition trueValue falseValue ->
            let conditionOutput = output condition
                trueOutput = "\n" ++ indent ++ "? " ++ output trueValue
                falseOutput = "\n" ++ indent ++ ": " ++ output falseValue
            in conditionOutput ++ trueOutput ++ falseOutput
        CaseOf variable cases ->
            let 
                outputCase previous (Case name expr) = previous ++ "\n" ++ indent ++ "case " ++ name ++ ": return " ++ outputIndented expr
                casesOutput = foldl' outputCase "" cases
            in "(() => { switch (" ++ output variable ++ ".constructor) {" ++ casesOutput ++ "\n" ++ base ++ "}})()"
        Binary left op right ->
            let leftOutput = output left
                rightOutput = output right
            in case op of
                Plus -> leftOutput ++ " + " ++ rightOutput
                Equality -> leftOutput ++ " === " ++ rightOutput
                _ -> ""
        Call callee arguments ->
            let args = map output arguments
            in output callee ++ "(" ++ intercalate ", " args ++ ")"
        FieldAccess record field ->
            output record ++ "." ++ toJS field
        Literal literal -> case literal of
            Integer integer -> show integer
            Lambda _ (Function params _ expr) ->
                let header = "(" ++ intercalate ", " (map (toJS . fst) params) ++ ") => "
                    body = outputIndented expr
                in header ++ body
            _ -> ""
        Identifier identifier _ -> toJS identifier
        None -> ""

toJS :: String -> String
toJS = ('$' :)

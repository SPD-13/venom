module Transpiler where

import Data.List (foldl', intercalate)

import AST
import Operator
import Position

var = "const "
tab = "    "

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
    in "function " ++ name ++ "(" ++ params ++ ") {" ++ setters ++ "\n}\n"

outputExpression :: Int -> Expression -> String
outputExpression tabLevel expression =
    let output = outputExpression tabLevel
        outputIndented = outputExpression $ tabLevel + 2
        base = concat $ replicate tabLevel tab
        single = concat $ replicate (tabLevel + 1) tab
        double = concat $ replicate (tabLevel + 2) tab
    in case expression of
        Let bindings expr ->
            let outputSetter (Binding name value _) = "\n" ++ single ++ "set(\"" ++ toJS name ++ "\", new $Thunk(() => " ++ output value ++ "))"
                setters = concatMap outputSetter bindings
                result = "\n" ++ single ++ "return " ++ output expr
            in "(() => {" ++ setters ++ result ++ "\n" ++ base ++ "})()"
        If condition trueValue falseValue ->
            let conditionOutput = output condition
                trueOutput = "\n" ++ single ++ "? " ++ output trueValue
                falseOutput = "\n" ++ single ++ ": " ++ output falseValue
            in conditionOutput ++ trueOutput ++ falseOutput
        CaseOf variable cases ->
            let 
                outputCase previous (Case name expr) = previous ++ "val.constructor === " ++ name ++ " ? " ++ outputIndented expr ++ "\n" ++ double ++ ": "
                casesOutput = foldl' outputCase "" cases ++ "null"
            in "(() => {\n" ++ single ++ var ++ "val = " ++ output variable ++ "\n" ++ single ++ "return " ++ casesOutput ++ "\n" ++ base ++ "})()"
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
                    outputSetter (name, _) = "\n" ++ double ++ "set(\"" ++ toJS name ++ "\", " ++ toJS name ++ ")"
                    setters = concatMap outputSetter params
                    result = "\n" ++ double ++ "return " ++ outputIndented expr
                    body = "(() => {" ++ setters ++ result ++ "\n" ++ single ++ "})()"
                in header ++ body
            _ -> ""
        Identifier identifier _ -> "get(\"" ++ toJS identifier ++ "\")"
        None -> ""

toJS :: String -> String
toJS = ('$' :)

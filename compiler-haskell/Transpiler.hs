module Transpiler where

import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import AST
import Operator
import Position

var = "let "
tab = "  "

transpile :: AST -> String
transpile (AST types bindings) =
    let constructorAlias = "_ = 'constructor'\n"
        constructors = concatMap outputConstructors types
        globalExpr = Let bindings $ Identifier "main" $ Position 0 0
        identifiers = S.toList $ S.fromList $ findIdentifiers globalExpr
        dictionary = M.fromList $ zip identifiers $ iterate nextIdentifierSafe "a"
        app = outputExpression dictionary 0 globalExpr
        output = "console.log(" ++ app ++ ")\n"
    in constructorAlias ++ constructors ++ output

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

toJS :: String -> String
toJS = ('$' :)

outputExpression :: M.Map String String -> Int -> Expression -> String
outputExpression dictionary tabLevel expression =
    let output = outputExpression dictionary tabLevel
        outputIndented = outputExpression dictionary $ tabLevel + 1
        base = concat $ replicate tabLevel tab
        indent = concat $ replicate (tabLevel + 1) tab
        toJS = translate dictionary
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
            in "(() => { switch (" ++ output variable ++ "[_]) {" ++ casesOutput ++ "\n" ++ base ++ "}})()"
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

findIdentifiers :: Expression -> [String]
findIdentifiers expression = case expression of
    Let bindings expr ->
        let getName (Binding name _ _) = name
            getValue (Binding _ value _) = value
            bindingNames = map getName bindings
            bindingIdentifiers = concatMap (findIdentifiers . getValue) bindings
            exprIdentifiers = findIdentifiers expr
        in bindingNames ++ exprIdentifiers ++ bindingIdentifiers
    If condition trueValue falseValue ->
        findIdentifiers condition ++ findIdentifiers trueValue ++ findIdentifiers falseValue
    CaseOf variable cases ->
        let getValue (Case _ value) = value
            variableIdentifiers = findIdentifiers variable
            caseIdentifiers = concatMap (findIdentifiers . getValue) cases
        in variableIdentifiers ++ caseIdentifiers
    Binary left _ right ->
        findIdentifiers left ++ findIdentifiers right
    Call callee arguments ->
        findIdentifiers callee ++ concatMap findIdentifiers arguments
    FieldAccess record _ ->
        findIdentifiers record
    Literal literal -> case literal of
        Lambda _ (Function params _ expr) ->
            map fst params ++ findIdentifiers expr
        _ -> []
    Identifier _ _ -> []
    None -> []

translate :: M.Map String String -> String -> String
translate dictionary identifier =
    fromMaybe identifier $ M.lookup identifier dictionary

-- JavaScript reserved keywords with 4 or fewer characters
keywords :: [String]
keywords = ["case", "do", "else", "for", "if", "in", "new", "this", "try", "var", "void", "with", "enum", "let", "byte", "char", "goto", "int", "long", "null", "true"]

nextIdentifierSafe :: String -> String
nextIdentifierSafe identifier =
    let next = nextIdentifier identifier
    in
        if next `elem` keywords
        then nextIdentifierSafe next
        else next

nextIdentifier :: String -> String
nextIdentifier identifier = case identifier of
    [] -> "a"
    head : tail -> case nextChar head of
        Just newHead -> newHead : tail
        Nothing -> 'a' : nextIdentifier tail

nextChar :: Char -> Maybe Char
nextChar char = case char of
    'z' -> Nothing
    _ -> Just $ succ char

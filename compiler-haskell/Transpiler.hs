module Transpiler where

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

import AST
import Operator
import Position

data TranspilerMode = Readable | Minified
data Settings = Settings
    { identifierToJS :: String -> String
    , constructorToJS :: String -> String
    , bindingString :: String
    , spaceString :: String
    , indentString :: String
    , newlineString :: String
    }

transpile :: TranspilerMode -> AST -> String
transpile mode (AST types bindings) =
    let typesIdentifiers = concatMap typeIdentifiers types
        globalExpr = Let bindings $ Identifier "app" $ Position 0 0
        bindingsIdentifiers = findIdentifiers globalExpr
        identifiers = S.toList $ S.fromList $ typesIdentifiers ++ bindingsIdentifiers
        dictionary = M.fromList $ zip identifiers $ iterate nextIdentifierSafe "a"
        settings = case mode of
            Readable -> Settings
                { identifierToJS = ('$' :)
                , constructorToJS = id
                , bindingString = "const "
                , spaceString = " "
                , indentString = "  "
                , newlineString = "\n"
                }
            Minified -> Settings
                { identifierToJS = translate dictionary
                , constructorToJS = translate dictionary . ('$' :)
                , bindingString = "let "
                , spaceString = ""
                , indentString = ""
                , newlineString = ""
                }
        space = spaceString settings
        nl = newlineString settings
        constructorAlias = "_" ++ space ++ "=" ++ space ++ "'constructor';" ++ nl
        constructors = concatMap (outputConstructors settings) types
        app = outputExpression settings 0 globalExpr
        output = "app = " ++ app ++ "\nconsole.log(app)" ++ nl
    in constructorAlias ++ constructors ++ output

typeIdentifiers :: TypeDeclaration -> [String]
typeIdentifiers (TypeDeclaration _ constructors) =
    concatMap constructorIdentifiers constructors

constructorIdentifiers :: Constructor -> [String]
constructorIdentifiers (Constructor name fields) =
    let getFieldName (Field fieldName _) = fieldName
        fieldNames = map getFieldName fields
    in name : ('$' : name) : fieldNames

outputConstructors :: Settings -> TypeDeclaration -> String
outputConstructors settings (TypeDeclaration _ constructors) =
    concatMap (outputConstructor settings) constructors

outputConstructor :: Settings -> Constructor -> String
outputConstructor settings (Constructor name fields) =
    let getName (Field fieldName _) = fieldName
        toJS = identifierToJS settings
        var = bindingString settings
        space = spaceString settings
        tab = indentString settings
        nl = newlineString settings
        fieldNames = map (toJS . getName) fields
        params = intercalate (',' : space) fieldNames
        outputSetter fieldName = nl ++ tab ++ "this." ++ fieldName ++ space ++ "=" ++ space ++ fieldName ++ ";"
        setters = concatMap outputSetter fieldNames
        typeName = constructorToJS settings name
        constructor = "function " ++ typeName ++ "(" ++ params ++ ")" ++ space ++ "{" ++ setters ++ nl ++ "}" ++ nl
        new = var ++ toJS name ++ space ++ "=" ++ space ++ "(...args)" ++ space ++ "=>" ++ space ++ "new " ++ typeName ++ "(...args);" ++ nl
    in constructor ++ new

outputExpression :: Settings -> Int -> Expression -> String
outputExpression settings tabLevel expression =
    let output = outputExpression settings tabLevel
        outputIndented = outputExpression settings $ tabLevel + 1
        toJS = identifierToJS settings
        var = bindingString settings
        space = spaceString settings
        tab = indentString settings
        nl = newlineString settings
        base = concat $ replicate tabLevel tab
        indent = concat $ replicate (tabLevel + 1) tab
    in case expression of
        Let bindings expr ->
            let outputSetter (Binding name value _) = nl ++ indent ++ var ++ toJS name ++ space ++ "=" ++ space ++ output value ++ ";"
                setters = concatMap outputSetter bindings
                result = nl ++ indent ++ "return " ++ output expr
            in "(()" ++ space ++ "=>" ++ space ++ "{" ++ setters ++ result ++ nl ++ base ++ "})()"
        If condition trueValue falseValue ->
            let conditionOutput = output condition
                trueOutput = nl ++ indent ++ "?" ++ space ++ output trueValue
                falseOutput = nl ++ indent ++ ":" ++ space ++ output falseValue
            in conditionOutput ++ trueOutput ++ falseOutput
        CaseOf variable cases ->
            let 
                outputCase (Case name expr) = nl ++ indent ++ "case " ++ constructorToJS settings name ++ ":" ++ space ++ "return " ++ outputIndented expr ++ ";"
                casesOutput = concatMap outputCase cases
            in "(()" ++ space ++ "=>" ++ space ++ "{" ++ space ++ "switch" ++ space ++ "(" ++ output variable ++ "[_])" ++ space ++ "{" ++ casesOutput ++ nl ++ base ++ "}})()"
        Binary left op right ->
            let leftOutput = output left
                rightOutput = output right
            in case op of
                Plus -> leftOutput ++ space ++ "+" ++ space ++ rightOutput
                Equality -> leftOutput ++ space ++ "===" ++ space ++ rightOutput
                _ -> ""
        Call callee arguments ->
            let args = map output arguments
            in output callee ++ "(" ++ intercalate (',' : space) args ++ ")"
        FieldAccess record field ->
            output record ++ "." ++ toJS field
        Literal literal -> case literal of
            Integer integer -> show integer
            Lambda _ (Function params _ expr) ->
                let header = "(" ++ intercalate (',' : space) (map (toJS . fst) params) ++ ")" ++ space ++ "=>" ++ space
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

module Transpiler where

import Data.List (intercalate)

import AST

transpile :: AST -> String
transpile (AST types bindings) =
    let constructors = concatMap outputConstructors types
    in constructors

outputConstructors :: TypeDeclaration -> String
outputConstructors (TypeDeclaration _ constructors) =
    concatMap outputConstructor constructors

outputConstructor :: Constructor -> String
outputConstructor (Constructor name fields) =
    let getName (Field fieldName _) = fieldName
        fieldNames = map (toJS . getName) fields
        params = intercalate ", " fieldNames
        outputSetter fieldName = "\n    this." ++ fieldName ++ " = " ++ fieldName
        setters = concatMap outputSetter fieldNames
    in "function " ++ name ++ "(" ++ params ++ ") {" ++ setters ++ "\n}\n"

toJS :: String -> String
toJS = ('$' :)

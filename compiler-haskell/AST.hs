module AST where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M

import Operator
import Position

type GenericParameter = String
type ConstructorName = String
type FreeVariable = String

data AST = AST [TypeDeclaration] [Binding]
data TypeDeclaration = TypeDeclaration String [GenericParameter] (NonEmpty Constructor)
data Constructor = Constructor String [Field]
data Field = Field String TypeAnnotation
data Binding = Binding String Expression ExpressionType

data TypeAnnotation
    = ConstantAnnotation String [TypeAnnotation]
    | FunctionAnnotation [GenericParameter] [TypeAnnotation] TypeAnnotation
    deriving Show

data ExpressionType
    = TInteger
    | TBool
    | TChar
    | TString
    | TCustom TypeInfo (Maybe FieldTypes)
    | TParameter String
    | TFunction [GenericParameter] [ExpressionType] ExpressionType
    | TUndefined
    deriving Eq

data TypeInfo = TypeInfo String [GenericParameter] [ConstructorName] deriving Eq

type FieldTypes = M.Map String ExpressionType

instance Show ExpressionType where
    show eType = case eType of
        TInteger -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
        TString -> "String"
        TCustom (TypeInfo name _ _) _ -> name
        TParameter name -> name
        TFunction genericParams paramTypes returnType ->
            let generics = if null genericParams then "" else "<" ++ intercalate ", " genericParams ++ ">"
            in generics ++ "(" ++ intercalate ", " (map show paramTypes) ++ ")" ++ show returnType
        TUndefined -> "Undefined"

data Expression
    = Let [Binding] Expression
    | If Expression Expression Expression
    | CaseOf Expression [Case]
    | Binary Expression Operator Expression
    | Call Expression [Expression]
    | FieldAccess Expression String
    | Literal Literal
    | Identifier String Position
    | None
    deriving Show

data Case = Case String Expression deriving Show

data Literal
    = Integer Integer
    | Bool Bool
    | Char Char
    | String String
    | Function [FreeVariable] [GenericParameter] [(String, TypeAnnotation)] TypeAnnotation Expression
    deriving Show

unlines' = init . unlines

indent :: String -> String
indent string =
    unlines' (map ("  " ++) (lines string))

instance Show AST where
    show (AST _ bindings) =
        "Bindings\n" ++ unlines' (map (indent . show) bindings)

instance Show Binding where
    show (Binding identifier expression bindingType) =
        "Binding " ++ show identifier ++ " " ++ show bindingType ++ "\n" ++ indent (show expression)

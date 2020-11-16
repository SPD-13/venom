module AST where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M

import Operator
import Position

data AST
    = AST [TypeDeclaration] [Binding]

data TypeDeclaration
    = TypeDeclaration String [String] (NonEmpty Constructor)
    deriving Eq

data Constructor
    = Constructor String [Field]
    deriving Eq

data Field
    = Field String TypeAnnotation
    deriving Eq

data Binding
    = Binding String Expression ExpressionType
    deriving Eq

data TypeAnnotation
    = ConstantAnnotation String
    | FunctionAnnotation [TypeAnnotation] TypeAnnotation
    deriving (Eq, Show)

data ExpressionType
    = TInteger
    | TBool
    | TChar
    | TString
    | TCustom TypeInfo (Maybe FieldTypes)
    | TFunction [ExpressionType] ExpressionType
    | TUndefined
    deriving Eq

data TypeInfo = TypeInfo String [String] deriving Eq

type FieldTypes = M.Map String ExpressionType

instance Show ExpressionType where
    show eType = case eType of
        TInteger -> "Int"
        TBool -> "Bool"
        TChar -> "Char"
        TString -> "String"
        TCustom (TypeInfo name _) _ -> name
        TFunction paramTypes functionType -> "(" ++ intercalate ", " (map show paramTypes) ++ ")" ++ show functionType
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
    deriving (Eq, Show)

data Case = Case String Expression deriving (Eq, Show)

data Literal
    = Integer Integer
    | Bool Bool
    | Char Char
    | String String
    | Lambda [String] Function
    deriving (Eq, Show)

data Function = Function [(String, TypeAnnotation)] TypeAnnotation Expression deriving (Eq, Show)

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

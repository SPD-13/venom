module AST where

import Data.List (intercalate)

import Operator
import Position

data AST
    = Bindings [Binding]

data Binding
    = Binding String Expression ExpressionType
    deriving Eq

data ExpressionType
    = TInteger
    | TBool
    | TChar
    | TString
    | TFunction [ExpressionType] ExpressionType
    | TUndefined
    deriving Eq

instance Show ExpressionType where
    show eType = case eType of
        TInteger -> "Integer"
        TBool -> "Bool"
        TChar -> "Char"
        TString -> "String"
        TFunction paramTypes functionType -> "(" ++ intercalate ", " (map show paramTypes) ++ ")" ++ show functionType
        TUndefined -> "Undefined"

data Expression
    = Let [Binding] Expression
    | If Expression Expression Expression
    | Binary Expression Operator Expression
    | Call Expression [Expression]
    | Literal Literal
    | Identifier String Position
    | None
    deriving (Eq, Show)

data Literal
    = Integer Integer
    | Bool Bool
    | Char Char
    | String String
    | Lambda [String] Function
    deriving (Eq, Show)

data Function = Function [(String, ExpressionType)] ExpressionType Expression deriving (Eq, Show)

unlines' = init . unlines

indent :: String -> String
indent string =
    unlines' (map ("  " ++) (lines string))

instance Show AST where
    show (Bindings bindings) =
        "Bindings\n" ++ unlines' (map (indent . show) bindings)

instance Show Binding where
    show (Binding identifier expression bindingType) =
        "Binding " ++ show identifier ++ " " ++ show bindingType ++ "\n" ++ indent (show expression)

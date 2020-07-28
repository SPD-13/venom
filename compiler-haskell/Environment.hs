module Environment (Env, new, set, get, Literal(..)) where

import Data.List (intercalate)
import qualified Data.Map as M

import AST (Expression)

data Literal
    = Integer Integer
    | Bool Bool
    | Function Env [String] Expression
    | RuntimeError

instance Show Literal where
    show (Integer a) = show a
    show (Bool a) = show a
    show (Function _ params _) = "Function(" ++ intercalate ", " params ++ ")"
    show RuntimeError = "RuntimeError"

instance Eq Literal where
    (Integer a) == (Integer b) = a == b
    (Bool a) == (Bool b) = a == b
    _ == _ = False

newtype Env = Env (M.Map String Literal)

instance Show Env where
    show (Env env) = intercalate "\n" $ map (\(k, v) -> k ++ " = " ++ show v) $ M.toList env

new :: Env
new = Env M.empty

set :: Env -> (String, Literal) -> Env
set (Env env) (identifier, value) = Env $ M.insert identifier value env

get :: String -> Env -> Maybe Literal
get identifier (Env env) = M.lookup identifier env

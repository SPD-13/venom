module Environment (Env, new, set, get, Computed(..), Value(..)) where

import Data.List (intercalate)
import qualified Data.Map as M

import AST (Expression)

data Computed
    = Integer Integer
    | Bool Bool
    | Function Env [String] Expression
    | RuntimeError

instance Show Computed where
    show (Integer a) = show a
    show (Bool a) = show a
    show (Function _ params _) = "Function(" ++ intercalate ", " params ++ ")"
    show RuntimeError = "RuntimeError"

instance Eq Computed where
    (Integer a) == (Integer b) = a == b
    (Bool a) == (Bool b) = a == b
    _ == _ = False

data Value
    = Expression Expression
    | Computed Computed
    deriving Show

newtype Env = Env (M.Map String Value)

instance Show Env where
    show (Env env) = intercalate "\n" $ map (\(k, v) -> k ++ " = " ++ show v) $ M.toList env

new :: Env
new = Env M.empty

set :: Env -> (String, Value) -> Env
set (Env env) (identifier, value) = Env $ M.insert identifier value env

get :: String -> Env -> Maybe Value
get identifier (Env env) = M.lookup identifier env

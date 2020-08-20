module Environment (Env, new, set, get, delete, Computed(..), Value(..)) where

import Data.List (intercalate)
import qualified Data.Map as M

import AST (Expression, Function(..))

data Computed
    = Integer Integer
    | Bool Bool
    | Closure Env Function
    | RuntimeError

instance Show Computed where
    show (Integer a) = show a
    show (Bool a) = show a
    show (Closure _ (Function params _)) = "Closure(" ++ intercalate ", " params ++ ")"
    show RuntimeError = "Runtime error"

instance Eq Computed where
    (Integer a) == (Integer b) = a == b
    (Bool a) == (Bool b) = a == b
    _ == _ = False

data Value
    = Expression Expression
    | Computed Computed

instance Show Value where
    show (Expression _) = "Not evaluated"
    show (Computed computed) = show computed

newtype Env = Env (M.Map String Value)

instance Show Env where
    show (Env env) = intercalate "\n" $ map (\(k, v) -> k ++ " = " ++ show v) $ M.toList env

new :: Env
new = Env M.empty

set :: Env -> (String, Value) -> Env
set (Env env) (identifier, value) = Env $ M.insert identifier value env

get :: String -> Env -> Maybe Value
get identifier (Env env) = M.lookup identifier env

delete :: Env -> String -> Env
delete (Env env) identifier = Env $ M.delete identifier env

module Environment (Env, new, set, get) where

import Data.List (intercalate)
import qualified Data.Map as M

import AST

newtype Env = Env (M.Map String Concrete)

instance Show Env where
    show (Env env) = intercalate "\n" $ map (\(k, v) -> k ++ " = " ++ show v) $ M.toList env

new :: Env
new = Env M.empty

set :: Env -> (String, Concrete) -> Env
set (Env env) (identifier, value) = Env $ M.insert identifier value env

get :: String -> Env -> Maybe Concrete
get identifier (Env env) = M.lookup identifier env

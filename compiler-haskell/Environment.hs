module Environment (Env, new, set, get) where

import Data.List (intercalate)
import qualified Data.Map as M

newtype Env = Env (M.Map String Integer)

instance Show Env where
    show (Env env) = intercalate "\n" $ map (\(k, v) -> k ++ " = " ++ show v) $ M.toList env

new :: Env
new = Env M.empty

set :: String -> Integer -> Env -> Env
set identifier value (Env env) = Env $ M.insert identifier value env

get :: String -> Env -> Maybe Integer
get identifier (Env env) = M.lookup identifier env

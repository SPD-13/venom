module Environment (Env, new, set, get, delete, Computed(..), Value(..)) where

import Data.List (intercalate)
import Control.Monad.ST

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B

import AST (Expression, Function(..))

type HashTable s k v = B.HashTable s k v

newtype Env s = Env (HashTable s String (Value s))

data Computed s
    = Integer Integer
    | Bool Bool
    | Closure (Env s) Function
    | RuntimeError

instance Show (Computed s) where
    show (Integer a) = show a
    show (Bool a) = show a
    show (Closure _ (Function params _)) = "Closure(" ++ intercalate ", " params ++ ")"
    show RuntimeError = "Runtime error"

instance Eq (Computed s) where
    (Integer a) == (Integer b) = a == b
    (Bool a) == (Bool b) = a == b
    _ == _ = False

data Value s
    = Expression Expression
    | Computed (Computed s)

new :: ST s (Env s)
new = fmap Env H.new

set :: Env s -> (String, Value s) -> ST s ()
set (Env env) (identifier, value) = H.insert env identifier value

get :: String -> Env s -> ST s (Maybe (Value s))
get identifier (Env env) = H.lookup env identifier

delete :: Env s -> String -> ST s ()
delete (Env env) identifier = H.delete env identifier

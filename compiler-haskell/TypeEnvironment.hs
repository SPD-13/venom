module TypeEnvironment (Env, new, copy, set, setRef, get, delete, Value(..)) where

import Data.List (intercalate)
import Control.Monad.ST
import Data.STRef

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B

import AST (Expression, ExpressionType, Function(..))

type HashTable s k v = B.HashTable s k v

newtype Env s = Env (HashTable s String (STRef s Value))

data Value
    = Expression Expression (Maybe ExpressionType)
    | Typed Expression ExpressionType

new :: ST s (Env s)
new = fmap Env H.new

copy :: Env s -> ST s (Env s)
copy (Env env) = do
    list <- H.toList env
    fmap Env $ H.fromList list

set :: Env s -> (String, Value) -> ST s ()
set (Env env) (identifier, value) = do
    ref <- newSTRef value
    H.insert env identifier ref

setRef :: Env s -> String -> STRef s Value -> ST s ()
setRef (Env env) identifier ref = H.insert env identifier ref

get :: String -> Env s -> ST s (Maybe (STRef s Value ))
get identifier (Env env) = H.lookup env identifier

delete :: Env s -> String -> ST s ()
delete (Env env) identifier = H.delete env identifier
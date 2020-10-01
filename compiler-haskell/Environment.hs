module Environment (Env, TypeEnv, new, copy, set, setRef, get, delete, Computed(..), Value(..), TypeValue(..)) where

import Data.List (intercalate)
import Control.Monad.ST
import Data.STRef

import qualified Data.HashTable.Class as H
import qualified Data.HashTable.ST.Basic as B

import AST (Expression, ExpressionType, Function(..))

type HashTable s k v = B.HashTable s k v
newtype GenericEnv s v = Env (HashTable s String (STRef s v))
type Env s = GenericEnv s (Value s)
type TypeEnv s = GenericEnv s TypeValue

data Computed s
    = Integer Integer
    | Bool Bool
    | Char Char
    | String String
    | Closure (Env s) Function
    | RuntimeError

instance Show (Computed s) where
    show (Integer a) = show a
    show (Bool a) = show a
    show (Char a) = show a
    show (String a) = show a
    show (Closure _ (Function params _ _)) = "Closure(" ++ intercalate ", " (map fst params) ++ ")"
    show RuntimeError = "Runtime error"

instance Eq (Computed s) where
    (Integer a) == (Integer b) = a == b
    (Bool a) == (Bool b) = a == b
    _ == _ = False

data Value s
    = Expression Expression
    | Computed (Computed s)

data TypeValue
    = Untyped Expression (Maybe ExpressionType)
    | Typed Expression ExpressionType

new :: ST s (GenericEnv s v)
new = fmap Env H.new

copy :: GenericEnv s v -> ST s (GenericEnv s v)
copy (Env env) = do
    list <- H.toList env
    fmap Env $ H.fromList list

set :: GenericEnv s v -> (String, v) -> ST s ()
set (Env env) (identifier, value) = do
    ref <- newSTRef value
    H.insert env identifier ref

setRef :: GenericEnv s v -> String -> STRef s v -> ST s ()
setRef (Env env) identifier ref = H.insert env identifier ref

get :: String -> GenericEnv s v -> ST s (Maybe (STRef s v))
get identifier (Env env) = H.lookup env identifier

delete :: GenericEnv s v -> String -> ST s ()
delete (Env env) identifier = H.delete env identifier

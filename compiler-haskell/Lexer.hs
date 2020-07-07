module Lexer where

import Token

data State = State
    { line :: Integer
    , column :: Integer
    , referenceIndent :: Maybe(Integer)
    , indentLevel :: Integer
    }

lex :: String -> [Token]
lex input =
    []

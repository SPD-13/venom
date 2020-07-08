module Main where

import Lexer

main :: IO ()
main = do
    let tokens = Lexer.lex "value = if test == 2 then 4 else 6"
    mapM_ print tokens

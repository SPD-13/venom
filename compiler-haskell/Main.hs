module Main where

import Lexer

main :: IO ()
main = do
    contents <- getContents
    let tokens = Lexer.lex contents
    mapM_ print tokens

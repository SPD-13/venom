module Main where

import Lexer
import Parser
import Interpreter

main :: IO ()
main = do
    contents <- getContents
    let tokens = Lexer.lex contents
    putStrLn "\n--- Lexer output ---\n"
    mapM_ print tokens
    let ast = Parser.parse tokens
    putStrLn "\n--- Parser output ---\n"
    print ast
    let result = Interpreter.interpret ast
    putStrLn "\n--- Interpreter output ---\n"
    print result

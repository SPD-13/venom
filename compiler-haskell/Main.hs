module Main where

import Lexer
import Parser
import TypeChecker
import Interpreter

main :: IO ()
main = do
    contents <- getContents
    let tokens = Lexer.lex contents
    --putStrLn "\n--- Lexer output ---\n"
    --mapM_ print tokens
    let ast = Parser.parse tokens
    --putStrLn "\n--- Parser output ---\n"
    --print ast
    let sortedAst = TypeChecker.check ast
    putStrLn "\n--- Type checker output ---\n"
    print sortedAst
    let result = Interpreter.interpret sortedAst
    putStrLn "\n--- Interpreter output ---\n"
    putStrLn result
    putStrLn ""

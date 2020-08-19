module Main where

import Lexer
import Parser
import Resolver
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
    let resolvedAST = Resolver.resolve ast
    let typeCheckedAST = TypeChecker.typeCheck resolvedAST
    --putStrLn "\n--- Type checker output ---\n"
    --print typeCheckedAST
    let result = Interpreter.interpret typeCheckedAST
    putStrLn "\n--- Interpreter output ---\n"
    putStrLn result
    putStrLn ""

module Main where

import Error
import Lexer
import Parser
import Resolver
import TypeChecker
import Interpreter

main :: IO ()
main = do
    contents <- getContents
    case Lexer.lex contents of
        Left errors -> putStr $ printErrors contents errors
        Right tokens -> do
            --putStrLn "\n--- Lexer output ---\n"
            --mapM_ print tokens
            case Parser.parse tokens of
                Left errors -> putStr $ printErrors contents errors
                Right ast -> do
                    --putStrLn "\n--- Parser output ---\n"
                    --print ast
                    case Resolver.resolve ast of
                        Left errors -> putStr $ printErrors contents errors
                        Right resolvedAST -> do
                            let typeCheckedAST = TypeChecker.typeCheck resolvedAST
                            --putStrLn "\n--- Type checker output ---\n"
                            --print typeCheckedAST
                            let result = Interpreter.interpret typeCheckedAST
                            putStrLn "\n--- Interpreter output ---\n"
                            putStrLn result

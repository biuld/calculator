module Main where

import Evaluator
import Lexer
import Parser
import Logger
import System.IO

main :: IO ()
main = runInterpreter False
  where 
    runInterpreter :: Bool -> IO ()
    runInterpreter showTree = do
      prompt
      input <- getLine
      case input of 
        ":quit" -> putStrLn "goodbye!\n"
        ":enableAST" -> do
          putStrLn "displaying Abstract Syntax Tree\n"
          runInterpreter True
        ":disableAST" -> do
          putStrLn "stop displaying Abstract Syntax Tree\n"
          runInterpreter False
        _ -> case do
          tokens <- lexx input
          res <- parse tokens
          return $ fst res of
            Left msg -> putStrLn msg
            Right e -> prettyPrint e showTree
          >> runInterpreter showTree

    prompt :: IO ()
    prompt = do 
        putStr "cal> "
        hFlush stdout
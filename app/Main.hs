module Main where

import Control.Monad.Except (MonadIO (liftIO), runExceptT)
import Control.Monad.State.Strict (evalState)
import Evaluator
import Lexer
import Logger
import Parser (parse)
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
        _ ->
          case do
            tokens <- lexx input
            evalState (runExceptT parse) tokens of
            Left msg -> putStrLn msg
            Right e -> prettyPrint e showTree
            >> runInterpreter showTree

    prompt :: IO ()
    prompt = do
      putStr "cal> "
      hFlush stdout
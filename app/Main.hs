module Main where

import Control.Monad.Trans
import Data.List (isPrefixOf)
import Evaluator
import Lexer
import Logger
import Parser (parse)
import System.Console.Haskeline
import System.IO

options = [":enableAST", ":disableAST", ":help", ":quit"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) options

settings =
  let c = completeWord Nothing " \t" $ return . searchFunc
   in setComplete c defaultSettings

main :: IO ()
main = runInputT settings (loop False)
  where
    loop :: Bool -> InputT IO ()
    loop showTree = do
      minput <- getInputLine "cal>"
      case minput of
        Nothing -> loop showTree
        Just ":quit" -> outputStrLn "goodbye! 😎\n"
        Just ":enableAST" -> do
          outputStrLn "displaying Abstract Syntax Tree\n"
          loop True
        Just ":disableAST" -> do
          outputStrLn "stop displaying Abstract Syntax Tree\n"
          loop False
        Just ":help" -> do
          outputStrLn ":enableAST => to display the parse tree"
          outputStrLn ":disableAST => to stop displaying the parse tree"
          outputStrLn ":help => to display this message"
          outputStrLn ":quit => to say goodbye 👋\n"
          loop showTree
        Just other ->
          let res = do
                tokens <- lexx other
                parse tokens
           in case res of
                Left msg -> outputStrLn $ msg <> "\n"
                Right e -> lift $ prettyPrint e showTree
                >> loop showTree
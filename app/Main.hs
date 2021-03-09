module Main where

import Control.Monad.Trans
import Data.Map.Strict
import Evaluator
import Lexer
import Logger
import Parser
import System.Console.Haskeline
import System.IO
import Utils

main :: IO ()
main = runInputT settings (loop False empty)
  where
    loop :: Bool -> Map String Expr -> InputT IO ()
    loop showTree n = do
      minput <- getInputLine "\ESC[1;32m\STXcal> \ESC[0m\STX"
      case fmap trim minput of
        Nothing -> loop showTree n
        Just ":quit" -> outputStrLn "goodbye! 😎\n"
        Just ":enableAST" -> do
          outputStrLn "displaying Abstract Syntax Tree\n"
          loop True n
        Just ":disableAST" -> do
          outputStrLn "stop displaying Abstract Syntax Tree\n"
          loop False n
        Just ":help" -> do
          outputStrLn ":enableAST => to display the parse tree"
          outputStrLn ":disableAST => to stop displaying the parse tree"
          outputStrLn ":help => to display this message"
          outputStrLn ":quit => to say goodbye 👋\n"
          loop showTree n
        Just ":context" -> do
          lift $ print n
          loop showTree n
        Just other ->
          case lexx other of
            Left msg -> outputStrLn (msg <> "\n") >> loop showTree n
            Right t ->
              case parse t n of
                (Left msg', _) -> outputStrLn (msg' <> "\n") >> loop showTree n
                (Right e, n') -> lift (prettyPrint e n' showTree) >> loop showTree n'
module Main where

import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Map
import Evaluator
import Lexer
import Logger
import Optics
import Parser
import System.Console.Haskeline
import Utils

main :: IO ()
main = runInputT settings (loop False emptyContext)
  where
    cal :: String -> Context -> Either String Context
    cal input c = do
      t <- lexx input
      return $ execState (runExceptT (do parse; eval)) (c & tokens .~ t)

    loop :: Bool -> Context -> InputT IO ()
    loop showTree c = do
      minput <- getInputLine "\ESC[1;32m\STXcal> \ESC[0m\STX"
      case fmap trim minput of
        Nothing -> loop showTree c
        Just ":quit" -> outputStrLn "goodbye! 😎\n"
        Just ":enableAST" -> do
          outputStrLn "displaying Abstract Syntax Tree\n"
          loop True c
        Just ":disableAST" -> do
          outputStrLn "stop displaying Abstract Syntax Tree\n"
          loop False c
        Just ":help" -> do
          outputStrLn ":context => to display calculation context"
          outputStrLn ":enableAST => to display the parse tree"
          outputStrLn ":disableAST => to stop displaying the parse tree"
          outputStrLn ":help => to display this message"
          outputStrLn ":quit => to say goodbye 👋\n"
          loop showTree c
        Just ":context" -> do
          outputStrLn $ show c <> "\n"
          loop showTree c
        Just other ->
          case cal other c of
            Left msg -> outputStrLn (msg <> "\n") >> loop showTree c
            Right c'@Context {_tree = tr, _root = r} ->
              let pp = do
                    putStrLn $ disp r
                    when showTree (prettyPrint tr)
                    putStrLn ""
               in lift pp >> loop showTree c'
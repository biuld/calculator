module Main where

import Common
import Control.Monad
import Control.Monad.Trans
import Logger
import System.Console.Haskeline
import Utils

main :: IO ()
main = runInputT settings (loop False emptyContext)
  where
    cal s c showTree fn = case fn s c of
      (Left msg, _) -> outputStrLn (msg <> "\n") >> loop showTree c
      (Right v, c'@Context {_tree = e}) -> do
        outputStrLn $ disp v
        lift $ when showTree (prettyPrint e)
        loop showTree c'

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
        Just (':' : 'r' : 'e' : 'a' : 'd' : ' ' : f) -> do
          s <- lift $ readFile f
          cal s c showTree xdF
        Just ":context" -> do
          outputStrLn $ show c <> "\n"
          loop showTree c
        Just s -> cal s c showTree xd

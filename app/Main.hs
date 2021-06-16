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

evalHelper :: String -> Either String Expr
evalHelper input = do
  t <- lexx input
  evalState (runExceptT parse) (emptyContext & tokens .~ t)

main :: IO ()
main = runInputT settings (loop False emptyContext)
  where
    cal :: String -> Context -> (Either String Expr, Context)
    cal input c =
      case lexx input of
        Left msg -> (Left msg, c)
        Right t ->
          let chain = do
                tr <- parse
                r <- eval
                c <- get
                put (c & tree .~ tr) -- eval changes tree by default, put tr back for prettyPrint
                return r
           in runState (runExceptT chain) (c & tokens .~ t)

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
            (Left msg, _) -> outputStrLn (msg <> "\n") >> loop showTree c
            (Right e, c'@Context {_tree = tr}) ->
              let pp = do
                    putStrLn $ disp e
                    when showTree (prettyPrint tr)
                    putStrLn ""
               in lift pp >> loop showTree c'
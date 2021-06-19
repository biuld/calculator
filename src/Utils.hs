module Utils where

import Common
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Evaluator
import Lexer
import Parser
import System.Console.Haskeline

chain input = lexx input >> parse >> eval

xd :: String -> Either String Expr
xd input = evalState (runExceptT $ chain input) emptyContext

xd' :: [Char] -> Context -> (Either String Expr, Context)
xd' input = runState (runExceptT $ chain input)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

options = [":enableAST", ":disableAST", ":help", ":quit", ":context", ":read"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) options

settings :: Settings IO
settings =
  let c = completeWord Nothing " \t" $ return . searchFunc
   in setComplete c defaultSettings

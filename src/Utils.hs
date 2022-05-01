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

chain p = lexx >> p >> eval

xd = runState (runExceptT $ chain parse)

xdF = runState (runExceptT $ chain parseF)

xdP input = evalState (runExceptT $ chain parse) (emptyContext{raw = input})

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

options = [":enableAST", ":disableAST", ":help", ":quit", ":context", ":read"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) options

settings :: Settings IO
settings =
  let c = completeWord Nothing " \t" $ return . searchFunc
   in setComplete c defaultSettings

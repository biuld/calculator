module Utils where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char
import Data.List
import System.Console.Haskeline

type Pack a b = ExceptT String (State a) b

class Show a => Display a where
  disp :: a -> String
  disp a = show a

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

options = [":enableAST", ":disableAST", ":help", ":quit", ":context"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) options

settings :: Settings IO
settings =
  let c = completeWord Nothing " \t" $ return . searchFunc
   in setComplete c defaultSettings

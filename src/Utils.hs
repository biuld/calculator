module Utils where

import Data.Char
import Data.List
import System.Console.Haskeline

class Show a => Display a where
  disp :: a -> String
  disp a = show a

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

options = [":enableAST", ":disableAST", ":help", ":quit"]

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) options

settings :: Settings IO
settings =
  let c = completeWord Nothing " \t" $ return . searchFunc
   in setComplete c defaultSettings

module Lexer where

import Data.Char (digitToInt, isDigit)

data Syntax
  = I Int
  | Add
  | Mul
  | Div
  | Sub
  | OpenPth
  | ClosePth
  | Space
  deriving (Show)

lexx :: String -> [Syntax]
lexx [] = []
lexx xs =
  case buildISyntax xs of
    (Nothing, x : tail) -> buildSyntax x : lexx tail
    (Just a, tail) -> a : lexx tail
  where
    buildSyntax :: Char -> Syntax
    buildSyntax '*' = Mul
    buildSyntax '+' = Add
    buildSyntax ' ' = Space
    buildSyntax '/' = Div
    buildSyntax '-' = Sub
    buildSyntax '(' = OpenPth
    buildSyntax ')' = ClosePth
    buildSyntax c = error $ show c

    buildISyntax :: String -> (Maybe Syntax, String)
    buildISyntax xs =
      case span isDigit xs of
        ([], tail) -> (Nothing, tail)
        (num, tail) -> (Just . I $ read num, tail)
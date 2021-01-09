module Lexer where

import Data.Char (digitToInt, isDigit)

data Token
  = I Int
  | Add
  | Mul
  | Div
  | Sub
  | OpenPth
  | ClosePth
  | Space
  deriving (Show)

lexx :: String -> [Token]
lexx [] = []
lexx xs =
  case getIToken xs of
    (Nothing, x : tail) -> 
      case getToken x of
        Space -> lexx tail
        t -> t:lexx tail
    (Just a, tail) -> a : lexx tail
  where
    getToken :: Char -> Token
    getToken '*' = Mul
    getToken '+' = Add
    getToken ' ' = Space
    getToken '/' = Div
    getToken '-' = Sub
    getToken '(' = OpenPth
    getToken ')' = ClosePth
    getToken c = error $ show c

    getIToken :: String -> (Maybe Token, String)
    getIToken xs =
      case span isDigit xs of
        ([], tail) -> (Nothing, tail)
        (num, tail) -> (Just . I $ read num, tail)
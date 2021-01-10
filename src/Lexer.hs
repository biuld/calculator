module Lexer where

import Data.Char (digitToInt, isDigit, isLetter)

data Token
  = I Int
  | B Bool
  | Add
  | Mul
  | Div
  | Sub
  | Equal
  | And
  | Or
  | Not
  | Space
  | OpenPth
  | ClosePth
  deriving (Show)

lexx :: String -> [Token]
lexx [] = []
lexx xs@(h:tail) 
  | isDigit h = 
    case getIToken xs of
      (token, rst) -> token:lexx rst
  | isLetter h = 
    case getKeywordToken xs of
      (token, rst) -> token:lexx rst
  | otherwise = 
    case getToken h of
      Space -> lexx tail
      token -> token:lexx tail
  where
    getToken :: Char -> Token
    getToken '*' = Mul
    getToken '+' = Add
    getToken ' ' = Space
    getToken '/' = Div
    getToken '-' = Sub
    getToken '(' = OpenPth
    getToken ')' = ClosePth
    getToken c = error $ show c <> " is not a valid token"

    getIToken :: String -> (Token, String)
    getIToken xs =
      case span isDigit xs of
        (num, tail) -> (I $ read num, tail)

    getKeywordToken :: String -> (Token, String)
    getKeywordToken xs =
      case span isLetter xs of
        ("true", tail) -> (B True, tail)
        ("false", tail) -> (B False, tail)
        ("and", tail) -> (And, tail)
        ("or", tail) -> (Or, tail)
        ("not", tail) -> (Not, tail)
        ("eq", tail) -> (Equal, tail)
        (other, _) -> error $ show other <> " is not a valid token"
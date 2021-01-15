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
  | NotEqual
  | And
  | Or
  | Not
  | Space
  | OpenPth
  | ClosePth
  deriving (Show)

lexx :: String -> [Token]
lexx [] = []
lexx xs@(h:_) 
  | isDigit h = 
    case getIToken xs of
      (token, tail) -> token:lexx tail
  | isLetter h = 
    case getKeywordToken xs of
      (token, tail) -> token:lexx tail
  | otherwise = 
    case getToken xs of
      (Space, tail) -> lexx tail
      (token, tail) -> token:lexx tail
  where
    getToken :: String -> (Token, String)
    getToken ('*':tail) = (Mul, tail)
    getToken ('+':tail) = (Add, tail)
    getToken (' ':tail) = (Space, tail)
    getToken ('/':tail) = (Div, tail)
    getToken ('-':tail) = (Sub, tail)
    getToken ('(':tail) = (OpenPth, tail)
    getToken (')':tail) = (ClosePth, tail)
    getToken ('=':'=':tail) = (Equal, tail)
    getToken ('!':'=':tail) = (NotEqual, tail)
    getToken ('&':'&':tail) = (And, tail)
    getToken ('|':'|':tail) = (Or, tail)
    getToken ('!':tail) = (Not, tail)
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
        (other, _) -> error $ show other <> " is not a valid token"
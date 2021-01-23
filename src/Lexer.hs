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

instance Show Token where
  show (I i) = show i
  show (B b) = show b
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Equal = "=="
  show NotEqual = "!="
  show And = "&&"
  show Or = "||"
  show Not = "!"
  show OpenPth = "("
  show ClosePth = ")"
  show Space = " "

lexx :: String -> Either String [Token]
lexx [] = return []
lexx xs@(h : _)
  | isDigit h = do
    (token, tail) <- getIToken xs
    rst <- lexx tail
    return $ token : rst
  | isLetter h = do
    (token, tail) <- getKeywordToken xs
    rst <- lexx tail
    return $ token : rst
  | otherwise = do
    (token, tail) <- getToken xs
    case token of
      Space -> lexx tail
      _ -> do
        rst <- lexx tail
        return $ token : rst
  where
    getToken :: String -> Either String (Token, String)
    getToken ('*' : tail) = return (Mul, tail)
    getToken ('+' : tail) = return (Add, tail)
    getToken (' ' : tail) = return (Space, tail)
    getToken ('/' : tail) = return (Div, tail)
    getToken ('-' : tail) = return (Sub, tail)
    getToken ('(' : tail) = return (OpenPth, tail)
    getToken (')' : tail) = return (ClosePth, tail)
    getToken ('=' : '=' : tail) = return (Equal, tail)
    getToken ('!' : '=' : tail) = return (NotEqual, tail)
    getToken ('&' : '&' : tail) = return (And, tail)
    getToken ('|' : '|' : tail) = return (Or, tail)
    getToken ('!' : tail) = return (Not, tail)
    getToken c = Left $ show c <> " is not a valid token"

    getIToken :: String -> Either String (Token, String)
    getIToken xs =
      case span isDigit xs of
        (num, tail) -> return (I $ read num, tail)

    getKeywordToken :: String -> Either String (Token, String)
    getKeywordToken xs =
      case span isLetter xs of
        ("true", tail) -> return (B True, tail)
        ("false", tail) -> return (B False, tail)
        (other, _) -> Left $ show other <> " is not a valid token"
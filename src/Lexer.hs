module Lexer where

import Data.Char (digitToInt, isDigit, isLetter)
import Utils

data Token
  = I Int
  | B Bool
  | N String
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
  | Separator
  | OpenPth
  | ClosePth
  | Ift
  | Elt
  | Assign
  | Let
  | OpenBracket
  | CloseBracket
  deriving (Eq, Show)

instance Display Token where
  disp (I i) = show i
  disp (B b) = show b
  disp (N n) = n
  disp Add = "+"
  disp Sub = "-"
  disp Mul = "*"
  disp Div = "/"
  disp Equal = "=="
  disp NotEqual = "!="
  disp And = "&&"
  disp Or = "||"
  disp Not = "!"
  disp OpenPth = "("
  disp ClosePth = ")"
  disp Space = " "
  disp Separator = ";"
  disp Ift = "if"
  disp Elt = "else"
  disp Assign = "="
  disp Let = "let"
  disp OpenBracket = "{"
  disp CloseBracket = "}"

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
    getToken ('\n' : tail) = return (Separator, tail)
    getToken ('\r' : tail) = return (Separator, tail)
    getToken (';' : tail) = return (Separator, tail)
    getToken ('/' : tail) = return (Div, tail)
    getToken ('-' : tail) = return (Sub, tail)
    getToken ('(' : tail) = return (OpenPth, tail)
    getToken (')' : tail) = return (ClosePth, tail)
    getToken ('=' : '=' : tail) = return (Equal, tail)
    getToken ('!' : '=' : tail) = return (NotEqual, tail)
    getToken ('&' : '&' : tail) = return (And, tail)
    getToken ('|' : '|' : tail) = return (Or, tail)
    getToken ('!' : tail) = return (Not, tail)
    getToken ('=' : tail) = return (Assign, tail)
    getToken ('{' : tail) = return (OpenBracket, tail)
    getToken ('}' : tail) = return (CloseBracket, tail)
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
        ("if", tail) -> return (Ift, tail)
        ("else", tail) -> return (Elt, tail)
        ("let", tail) -> return (Let, tail)
        (other, tail) -> return (N other, tail)
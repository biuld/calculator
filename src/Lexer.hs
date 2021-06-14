module Lexer where

import Control.Applicative
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
  | OpenPth
  | ClosePth
  | Ift
  | Elt
  | Assign
  | Let
  | CommaSep
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
  disp Ift = "if"
  disp Elt = "else"
  disp Assign = "="
  disp Let = "let"
  disp CommaSep = ","

lexx :: String -> Either String [Token]
lexx [] = return []
lexx xs@(h : _) = do
  (token, tail) <- getIToken xs <|> getKeywordToken xs <|> getToken xs
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
    getToken ('=' : tail) = return (Assign, tail)
    getToken (',' : tail) = return (CommaSep, tail)
    getToken c = Left $ show c <> " is not a valid token"

    getIToken :: String -> Either String (Token, String)
    getIToken [] = Left ""
    getIToken xs@(h : _)
      | isDigit h = return (I $ read num, tail)
      | otherwise = Left ""
      where
        (num, tail) = span isDigit xs

    getKeywordToken :: String -> Either String (Token, String)
    getKeywordToken [] = Left ""
    getKeywordToken xs@(h : _)
      | isLetter h =
        case span isLetter xs of
          ("true", tail) -> return (B True, tail)
          ("false", tail) -> return (B False, tail)
          ("if", tail) -> return (Ift, tail)
          ("else", tail) -> return (Elt, tail)
          ("let", tail) -> return (Let, tail)
          (other, tail) -> return (N other, tail)
      | otherwise = Left ""
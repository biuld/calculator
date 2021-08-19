module Lexer where

import Common
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Char (isDigit, isLetter)
import Optics

lexx :: App ()
lexx = do
  c@Context {_raw = input} <- get
  t <- loop input
  put (c & tokens .~ t)

loop :: String -> App [Token]
loop [] = return []
loop xs@(h : _) = do
  (token, tail) <- getIToken xs <|> getKeywordToken xs <|> getToken xs
  case token of
    Space -> loop tail
    _ -> do
      rst <- loop tail
      return $ token : rst

getToken :: String -> App (Token, String)
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
getToken ('\n' : tail) = return (Space, tail)
getToken ('\r' : tail) = return (Space, tail)
getToken ('\t' : tail) = return (Space, tail)
getToken (';' : tail) = return (LineSep, tail)
getToken ('{' : tail) = return (OpenBracket, tail)
getToken ('}' : tail) = return (CloseBracket, tail)
getToken c = throwError $ show c <> " is not a valid token"

getIToken :: String -> App (Token, String)
getIToken [] = throwError ""
getIToken xs@(h : _)
  | isDigit h = return (I $ read num, tail)
  | otherwise = throwError ""
  where
    (num, tail) = span isDigit xs

getKeywordToken :: String -> App (Token, String)
getKeywordToken [] = throwError ""
getKeywordToken xs@(h : _)
  | isLetter h =
    case span isLetter xs of
      ("true", tail) -> return (B True, tail)
      ("false", tail) -> return (B False, tail)
      ("if", tail) -> return (Ift, tail)
      ("else", tail) -> return (Elt, tail)
      ("let", tail) -> return (Let, tail)
      ("def", tail) -> return (Def, tail)
      ("return", tail) -> return (Ret, tail)
      (other, tail) -> return (N other, tail)
  | otherwise = throwError ""
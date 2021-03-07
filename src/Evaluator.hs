module Evaluator where

import Lexer
import Parser
import Utils

binErrMsg :: Token -> Expr -> Expr -> String
binErrMsg op l r =
  disp op
    <> " is not defined for "
    <> disp l
    <> " and "
    <> disp r

unErrMsg :: Token -> Expr -> String
unErrMsg op e =
  disp op
    <> " is not defined for "
    <> disp e

eval :: Expr -> Either String Expr
eval (Figure i) = return $ Figure i
eval (Boolean b) = return $ Boolean b
eval Unit = return Unit
eval (Pth e) = eval e
eval (If b l r) = do
  bv <- eval b
  case bv of
    Boolean bb -> if bb then eval l else eval r
    n -> Left $ "expected a boolean condition in if expression, got " <> disp n
eval (Binary op l r) = do
  lv <- eval l
  rv <- eval r
  case (op, lv, rv) of
    (Add, Figure li, Figure ri) -> return . Figure $ li + ri
    (Sub, Figure li, Figure ri) -> return . Figure $ li - ri
    (Mul, Figure li, Figure ri) -> return . Figure $ li * ri
    (Div, Figure li, Figure ri) -> return . Figure $ li `div` ri
    (Equal, Figure li, Figure ri) -> return . Boolean $ li == ri
    (Equal, Boolean lb, Boolean rb) -> return . Boolean $ lb == rb
    (Equal, Figure _, Boolean _) -> return $ Boolean False
    (Equal, Boolean _, Figure _) -> return $ Boolean False
    (NotEqual, Figure li, Figure ri) -> return . Boolean $ li /= ri
    (NotEqual, Boolean lb, Boolean rb) -> return . Boolean $ lb /= rb
    (NotEqual, Figure _, Boolean _) -> return $ Boolean True
    (NotEqual, Boolean _, Figure _) -> return $ Boolean True
    (And, Boolean lb, Boolean rb) -> return . Boolean $ lb && rb
    (Or, Boolean lb, Boolean rb) -> return . Boolean $ lb || rb
    (t, ll, rr) -> Left $ binErrMsg t ll rr
eval (Unary op e) = do
  ev <- eval e
  case (op, ev) of
    (Add, Figure i) -> return $ Figure i
    (Sub, Figure i) -> return . Figure $ - i
    (Not, Boolean b) -> return . Boolean $ not b
    (t, ee) -> Left $ unErrMsg t ee
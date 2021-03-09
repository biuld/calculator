module Evaluator where

import Data.Map.Strict as M
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

eval :: Expr -> Map String Expr -> Either String Expr
eval (Figure i) n = return $ Figure i
eval (Boolean b) n = return $ Boolean b
eval (Name v) n =
  case M.lookup v n of
    Just e -> eval e n
    Nothing -> Left $ "variable " <> v <> " is undefined"
eval Unit n = return Unit
eval (Pth e) n = eval e n
eval (If b l r) n = do
  bv <- eval b n
  case bv of
    Boolean bb -> if bb then eval l n else eval r n
    n -> Left $ "expected a boolean condition in if expression, got " <> disp n
eval (Binary op l r) n = do
  lv <- eval l n
  rv <- eval r n
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
eval (Unary op e) n = do
  ev <- eval e n
  case (op, ev) of
    (Add, Figure i) -> return $ Figure i
    (Sub, Figure i) -> return . Figure $ - i
    (Not, Boolean b) -> return . Boolean $ not b
    (t, ee) -> Left $ unErrMsg t ee
eval (Bind _ e) n = eval e n
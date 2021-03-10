module Evaluator where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict as M
import Lexer
import Optics
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

eval :: Pack Context ()
eval = do
  c@Context {_tree = tr} <- get
  case tr of
    Figure i -> put (c & root .~ Figure i)
    Boolean b -> put (c & root .~ Boolean b)
    Unit -> put (c & root .~ Unit)
    Pth e -> delegate e c
    Bind _ e -> delegate e c
    Name n -> do
      Context {_names = names} <- get
      case M.lookup n names of
        Just e -> delegate e c
        Nothing -> put (c & root .~ Unit)
    If b l r -> do
      bv <- deduce b c
      case bv of
        Boolean bb ->
          let h = if bb then l else r in delegate h c
        n -> throwError $ "expected a boolean condition in if expression, got " <> disp n
    Unary op e -> do
      ev <- deduce e c
      case (op, ev) of
        (Add, Figure i) -> put (c & root .~ Figure i)
        (Sub, Figure i) -> put (c & root .~ Figure (- i))
        (Not, Boolean b) -> put (c & root .~ Boolean (not b))
        (t, ee) -> throwError $ unErrMsg t ee
    Binary op l r -> do
      lv <- deduce l c
      rv <- deduce r c
      case (op, lv, rv) of
        (Add, Figure li, Figure ri) -> put (c & root .~ Figure (li + ri))
        (Sub, Figure li, Figure ri) -> put (c & root .~ Figure (li - ri))
        (Mul, Figure li, Figure ri) -> put (c & root .~ Figure (li * ri))
        (Div, Figure li, Figure ri) -> put (c & root .~ Figure (li `div` ri))
        (Equal, Figure li, Figure ri) -> put (c & root .~ Boolean (li == ri))
        (Equal, Boolean lb, Boolean rb) -> put (c & root .~ Boolean (lb == rb))
        (NotEqual, Figure li, Figure ri) -> put (c & root .~ Boolean (li /= ri))
        (NotEqual, Boolean lb, Boolean rb) -> put (c & root .~ Boolean (lb /= rb))
        (And, Boolean lb, Boolean rb) -> put (c & root .~ Boolean (lb && rb))
        (Or, Boolean lb, Boolean rb) -> put (c & root .~ Boolean (lb || rb))
        (t, ll, rr) -> throwError $ binErrMsg t ll rr
  where
    delegate :: Expr -> Context -> Pack Context ()
    delegate e c = do
      put (c & tree .~ e)
      eval

    deduce :: Expr -> Context -> Pack Context Expr
    deduce e c = do
      delegate e c
      Context {_root = r} <- get
      return r
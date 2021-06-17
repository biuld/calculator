module Evaluator where

import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict as M (fromList, insert, lookup, union)
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

eval :: Pack Context Expr
eval = do
  c@Context {_tree = tr} <- get
  case tr of
    Figure i -> return $ Figure i
    Boolean b -> return $ Boolean b
    Unit -> return Unit
    Pth e -> deduce c e
    Bind name e -> do
      c@Context {_names = n} <- get
      ee <- deduce c e
      put (c & names .~ M.insert name ee n)
      return ee
    Name n -> do
      Context {_names = names} <- get
      case M.lookup n names of
        Just e -> deduce c e
        Nothing -> return Unit
    If b l r -> do
      bv <- deduce c b
      case bv of
        Boolean bb ->
          let h = if bb then l else r in deduce c h
        n -> throwError $ "expected a boolean condition in if expression, got " <> disp n
    Unary op e -> do
      ev <- deduce c e
      case (op, ev) of
        (Add, Figure i) -> return $ Figure i
        (Sub, Figure i) -> return $ Figure (- i)
        (Not, Boolean b) -> return $ Boolean (not b)
        (t, ee) -> throwError $ unErrMsg t ee
    Binary op l r -> do
      lv <- deduce c l
      rv <- deduce c r
      case (op, lv, rv) of
        (Add, Figure li, Figure ri) -> return $ Figure (li + ri)
        (Sub, Figure li, Figure ri) -> return $ Figure (li - ri)
        (Mul, Figure li, Figure ri) -> return $ Figure (li * ri)
        (Div, Figure li, Figure ri) -> return $ Figure (li `div` ri)
        (Equal, Figure li, Figure ri) -> return $ Boolean (li == ri)
        (Equal, Boolean lb, Boolean rb) -> return $ Boolean (lb == rb)
        (NotEqual, Figure li, Figure ri) -> return $ Boolean (li /= ri)
        (NotEqual, Boolean lb, Boolean rb) -> return $ Boolean (lb /= rb)
        (And, Boolean lb, Boolean rb) -> return $ Boolean (lb && rb)
        (Or, Boolean lb, Boolean rb) -> return $ Boolean (lb || rb)
        (t, ll, rr) -> throwError $ binErrMsg t ll rr
    Group es -> go es c
    f@(FuncDef name _ _) -> do
      c@Context {_names = n} <- get
      put (c & names .~ M.insert name f n)
      return Unit
    FuncCall name ps -> do
      c@Context {_names = n} <- get
      case M.lookup name n of
        Just f@(FuncDef _ param bs) ->
          let n' = n `union` fromList (param `zip` ps)
              c' = emptyContext & names .~ n'
           in do
                es <- deduce c' $ Group bs
                let Group r = es in return $ last r
        _ -> throwError $ "function " <> name <> " is undefined"
  where
    deduce :: Context -> Expr -> Pack Context Expr
    deduce c e = do
      put (c & tree .~ e)
      eval

    go :: [Expr] -> Context -> Pack Context Expr
    go [] _ = return $ Group []
    go (h : tail) c = do
      e' <- deduce c h
      c' <- get
      next <- go tail c'
      let Group n = next in return $ Group (e' : n)

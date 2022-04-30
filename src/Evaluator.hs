{-# LANGUAGE OverloadedRecordDot #-}

module Evaluator where

import Common
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Map.Strict as M (fromList, insert, lookup)

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

eval :: App Expr
eval = do
  c <- get
  case c.value of
    Figure i -> return $ Figure i
    Boolean b -> return $ Boolean b
    Unit -> return Unit
    Pth e -> deduce c e
    Return e -> Return <$> deduce c e
    Bind name e -> do
      c@Context{names = n} <- get
      ee <- deduce c e
      put (c{names = M.insert name ee n})
      return ee
    Name n -> return $ search (n, c)
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
        (Sub, Figure i) -> return $ Figure (-i)
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
    Group es -> Group <$> go es c
    f@(FuncDef name ps _) -> do
      c@Context{names = n} <- get
      put (c{names = M.insert (signature name ps) f n})
      return Unit
    FuncCall name ps -> do
      ps' <- traverse (deduce c) ps
      c <- get
      let fn = signature name ps
      case search (fn, c) of
        f@(FuncDef _ param bs) ->
          let ns = fromList (param `zip` ps')
              c' = emptyContext{parent = Just $ c{names = ns}}
           in last <$> go bs c'
        _ -> throwError $ "function " <> fn <> " is undefined"
 where
  signature :: String -> [a] -> String
  signature name ps = name <> show (length ps)

  search :: (String, Context) -> Expr
  search (n, Context{names = names, parent = p}) =
    case M.lookup n names of
      Just e -> e
      Nothing -> case p of
        Just p' -> search (n, p')
        Nothing -> Unit

  deduce :: Context -> Expr -> App Expr
  deduce c e = do
    put (c{value = e})
    eval

  go :: [Expr] -> Context -> App [Expr]
  go [] _ = return []
  go (h : tail) c = do
    e <- deduce c h
    case e of
      Return e' -> return [e']
      _ -> do
        c' <- get
        next <- go tail c'
        return $ e : next

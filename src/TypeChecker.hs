module TypeChecker where

import Evaluator
import Lexer
import Parser
import Utils

data Type = BB | II | UU deriving (Eq, Show)

instance Display Type where
  disp BB = "Boolean"
  disp II = "Integer"
  disp UU = "Unit"

binErrMsg :: Token -> Expr -> Type -> Expr -> Type -> String
binErrMsg op l lt r rt =
  show op
    <> " is not defined for "
    <> show l
    <> " :: "
    <> show lt
    <> " and "
    <> show r
    <> " :: "
    <> show rt

unErrMsg :: Token -> Expr -> Type -> String
unErrMsg op e t =
  show op
    <> " is not defined for "
    <> show e
    <> " :: "
    <> show t

typeCheck :: Expr -> Either String Type
typeCheck (Figure _) = return II
typeCheck (Boolean _) = return BB
typeCheck Unit = return UU
typeCheck (Pth e) = typeCheck e
typeCheck (If b l r) =
  case eval b of
    Boolean bb -> if bb then typeCheck l else typeCheck r
    n -> Left $ "expected a boolean condition in if expression, got " <> disp n
typeCheck (Binary op l r) = do
  lt <- typeCheck l
  rt <- typeCheck r
  case (op, lt, rt) of
    (Add, II, II) -> return II
    (Sub, II, II) -> return II
    (Mul, II, II) -> return II
    (Div, II, II) -> return II
    (Equal, II, II) -> return BB
    (Equal, BB, BB) -> return BB
    (Equal, II, BB) -> return BB
    (Equal, BB, II) -> return BB
    (NotEqual, II, II) -> return BB
    (NotEqual, BB, BB) -> return BB
    (NotEqual, II, BB) -> return BB
    (NotEqual, BB, II) -> return BB
    (And, BB, BB) -> return BB
    (Or, BB, BB) -> return BB
    (_, _, _) -> Left $ binErrMsg op l lt r rt
typeCheck (Unary op e) = do
  t <- typeCheck e
  case (op, t) of
    (Add, II) -> return II
    (Sub, II) -> return II
    (Not, BB) -> return BB
    (_, t) -> Left $ unErrMsg op e t
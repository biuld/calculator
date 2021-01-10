module TypeChecker where

import Parser
import Lexer

data Type = BB | II 

instance Show Type where
  show BB = "Boolean"
  show II = "Integer"

binErrMsg :: Token -> Expr a -> Type -> Expr a -> Type -> String
binErrMsg op l lt r rt = show op
  <> " is not defined for \n"
  <> show l <> " which has a type of " <> show lt <> "\nand "
  <> show r <> " which has a type of " <> show rt

unErrMsg :: Token -> Expr a -> Type -> String
unErrMsg op e t = show op
  <> " is not defined for \n"
  <> show e <> " which has a type of " <> show t

typeCheck :: Expr a -> Type
typeCheck (Figure _) = II
typeCheck (Boolean _) = BB
typeCheck (Pth e) = typeCheck e
typeCheck (Binary op l r) =
    case (op, typeCheck l, typeCheck r) of
      (Add, II, II) -> II
      (Sub, II, II) -> II
      (Mul, II, II) -> II
      (Div, II, II) -> II
      (Equal, II, II) -> BB
      (Equal, BB, BB) -> BB
      (NotEqual, II, II) -> BB
      (NotEqual, BB, BB) -> BB
      (NotEqual, II, BB) -> BB
      (NotEqual, BB, II) -> BB
      (And, BB, BB) -> BB
      (Or, BB, BB) -> BB
      (_, lt, rt) -> error $ binErrMsg op l lt r rt
typeCheck (Unary op e) = 
    case (op, typeCheck e) of
      (Add, II) -> II
      (Sub, II) -> II
      (Not, BB) -> BB
      (_, t) -> error $ unErrMsg op e t
      
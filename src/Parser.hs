module Parser where

import Lexer

data Expr
  = AddOp Expr Expr
  | MulOp Expr Expr
  | SubOp Expr Expr
  | DivOp Expr Expr
  | Pth Expr
  | Num Int
  deriving (Show)

parse :: [Syntax] -> (Maybe Expr, [Syntax])
parse input = parseExpr $ filter notSpace input
  where
    parseExpr :: [Syntax] -> (Maybe Expr, [Syntax])
    parseExpr xs = parseTerm Nothing xs

    parseTerm :: Maybe Expr -> [Syntax] -> (Maybe Expr, [Syntax])
    parseTerm Nothing xs =
      case parseFactor Nothing xs of
        (val@(Just _), tail) -> parseTerm val tail
    parseTerm (Just e) xs@(Add : tail) =
      case parseFactor Nothing tail of
        (Just num, rst) -> parseTerm (Just $ AddOp e num) rst
    parseTerm (Just e) xs@(Sub : tail) =
      case parseFactor Nothing tail of
        (Just num, rst) -> parseTerm (Just $ SubOp e num) rst
    parseTerm val@(Just _) xs = (val, xs)

    parseFactor :: Maybe Expr -> [Syntax] -> (Maybe Expr, [Syntax])
    parseFactor Nothing xs =
      case parsePth xs of
        (val@(Just _), tail) -> parseFactor val tail
    parseFactor (Just e) xs@(Mul : tail) =
      case parsePth tail of
        (Just num, rst) -> parseFactor (Just $ MulOp e num) rst
    parseFactor (Just e) xs@(Div : tail) =
      case parsePth tail of
        (Just num, rst) -> parseFactor (Just $ DivOp e num) rst
    parseFactor val@(Just _) xs = (val, xs)

    parsePth :: [Syntax] -> (Maybe Expr, [Syntax])
    parsePth (OpenPth : tail) =
      case parseExpr tail of
        (Just e, ClosePth : rst) -> (Just $ Pth e, rst)
    parsePth xs = parseNum xs

    parseNum :: [Syntax] -> (Maybe Expr, [Syntax])
    parseNum ((I i) : tail) = (Just $ Num i, tail)
    parseNum xs = (Nothing, xs)

    notSpace :: Syntax -> Bool
    notSpace Space = False
    notSpace _ = True

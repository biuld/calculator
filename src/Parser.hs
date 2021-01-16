module Parser where

import Lexer

data Expr a
  = Figure Int
  | Boolean Bool
  | Pth (Expr a)
  | Unary Token (Expr a)
  | Binary Token (Expr a) (Expr a)

instance Show (Expr a) where
  show (Figure i) = show i
  show (Boolean b) = show b
  show (Pth e) = "( " <> show e <> " )"
  show (Binary op l r) = show l <> " " <> show op <> " " <> show r
  show (Unary op e) = show op <> " " <> show e

type Precedence = Int

getUnaryOpPrecedence :: Token -> Precedence
getUnaryOpPrecedence Add = 6
getUnaryOpPrecedence Sub = 6
getUnaryOpPrecedence Not = 6
getUnaryOpPrecedence _ = 0

getBinaryOpPrecedence :: Token -> Precedence
getBinaryOpPrecedence Mul = 5
getBinaryOpPrecedence Div = 5
getBinaryOpPrecedence Add = 4
getBinaryOpPrecedence Sub = 4
getBinaryOpPrecedence Equal = 3
getBinaryOpPrecedence NotEqual = 3
getBinaryOpPrecedence And = 2
getBinaryOpPrecedence Or = 1
getBinaryOpPrecedence _ = 0

parse :: [Token] -> Either String (Expr a, [Token])
parse input = do
  res <- parseExpr input 0
  tryRestart res
  where
    tryRestart :: (Expr a, [Token]) -> Either String (Expr a, [Token])
    tryRestart (l, []) = return (l, [])
    tryRestart (l, op : tail) = do
      res <- parseExpr tail (getBinaryOpPrecedence op)
      case res of
        (r, []) -> return (Binary op l r, [])
        (r, rst) -> 
          do
            (rr, rrst) <- tryRestart (r, rst)
            return (Binary op l rr, rrst)

    parseExpr :: [Token] -> Precedence -> Either String (Expr a, [Token])
    parseExpr xs@(Add:_) p = parseUnary xs p
    parseExpr xs@(Sub:_) p = parseUnary xs p
    parseExpr xs@(Not:_) p = parseUnary xs p
    parseExpr xs p = parseBinary xs p

    parseBinary :: [Token] -> Precedence -> Either String (Expr a, [Token])
    parseBinary xs p = do
      res <- parsePth xs
      case res of
        (l, []) -> return (l, [])
        (l, ys@(op : tail)) ->
          let precedence = getBinaryOpPrecedence op
            in if precedence > p
                then do
                  (r, rst) <- parseExpr tail precedence 
                  return (Binary op l r, rst)
                else return (l, ys)

    parseUnary :: [Token] -> Precedence -> Either String (Expr a, [Token])
    parseUnary (operator:tail) p =
      let precedence = getUnaryOpPrecedence operator 
          in if precedence >= p
            then do 
              (operand, rst) <- parseExpr tail precedence
              return (Unary operator operand, rst)
            else Left $ "Error token " <> show operator

    parsePth :: [Token] -> Either String (Expr a, [Token])
    parsePth (OpenPth : tail) = do
      res <- parseExpr tail 0 
      case res of 
        (e, ClosePth : rst) -> return (Pth e, rst)
    parsePth xs = parseLiteral xs

    parseLiteral :: [Token] -> Either String (Expr a, [Token])
    parseLiteral ((I i) : tail) = return (Figure i, tail)
    parseLiteral ((B b) : tail) = return (Boolean b, tail)
    parseLiteral (h : tail) = Left $ "expected literal expression, got " <> show h
    parseLiteral [] = Left "expected literal expression, got nothing"
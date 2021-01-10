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

parse :: [Token] -> (Expr a, [Token])
parse input = tryRestart $ parseExpr input 0
  where
    tryRestart :: (Expr a, [Token]) -> (Expr a, [Token])
    tryRestart (l, []) = (l, [])
    tryRestart (l, op : tail) = case parseExpr tail (getBinaryOpPrecedence op) of
      (r, []) -> (Binary op l r, [])
      (r, rst) -> 
        let 
          (rr, rrst) = tryRestart (r, rst)
        in (Binary op l rr, rrst)

    parseExpr :: [Token] -> Precedence -> (Expr a, [Token])
    parseExpr xs@(Add:_) p = parseUnary xs p
    parseExpr xs@(Sub:_) p = parseUnary xs p
    parseExpr xs@(Not:_) p = parseUnary xs p
    parseExpr xs p = parseBinary xs p

    parseBinary :: [Token] -> Precedence -> (Expr a, [Token])
    parseBinary xs p = 
      case parsePth xs of
        (l, []) -> (l, [])
        (l, ys@(op : tail)) ->
          let precedence = getBinaryOpPrecedence op
            in if precedence > p
                then case parseExpr tail precedence of
                  (r, rst) -> (Binary op l r, rst)
                else (l, ys)

    parseUnary :: [Token] -> Precedence -> (Expr a, [Token])
    parseUnary (operator:tail) p =
      let precedence = getUnaryOpPrecedence operator 
          in if precedence >= p
            then case parseExpr tail precedence of
              (operand, rst) -> (Unary operator operand, rst)
            else error $ "Error token " <> show operator

    parsePth :: [Token] -> (Expr a, [Token])
    parsePth (OpenPth : tail) =
      case parseExpr tail 0 of
        (e, ClosePth : rst) -> (Pth e, rst)
    parsePth xs = parseLiteral xs

    parseLiteral :: [Token] -> (Expr a, [Token])
    parseLiteral ((I i) : tail) = (Figure i, tail)
    parseLiteral ((B b) : tail) = (Boolean b, tail)
    parseLiteral (h : tail) = error $ "Error token " <> show h
    parseLiteral [] = error "got nothing when try to parse a literal expression"
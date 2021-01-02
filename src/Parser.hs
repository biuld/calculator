module Parser where

import Lexer

data Expr
  = AddOp Expr Expr
  | MulOp Expr Expr
  | SubOp Expr Expr
  | DivOp Expr Expr
  | Pth Expr
  | Unary Syntax Expr
  | Num Int
  deriving (Show)

type Precedence = Int

getUnaryOpPrecedence :: Syntax -> Precedence
getUnaryOpPrecedence Add = 3
getUnaryOpPrecedence Sub = 3
getUnaryOpPrecedence t = 0

getBinaryOpPrecedence :: Syntax -> Precedence
getBinaryOpPrecedence Mul = 2
getBinaryOpPrecedence Div = 2
getBinaryOpPrecedence Add = 1
getBinaryOpPrecedence Sub = 1
getBinaryOpPrecedence _ = 0

buildBinaryExpr :: Syntax -> Expr -> Expr -> Expr
buildBinaryExpr Add = AddOp
buildBinaryExpr Sub = SubOp
buildBinaryExpr Mul = MulOp
buildBinaryExpr Div = DivOp

parse :: [Syntax] -> (Expr, [Syntax])
parse input = tryRestart $ parseExpr (filter notSpace input) 0
  where
    tryRestart :: (Expr, [Syntax]) -> (Expr, [Syntax])
    tryRestart (l, []) = (l, [])
    tryRestart (l, op : tail) = case parseExpr tail (getBinaryOpPrecedence op) of
      (r, rst) -> tryRestart (buildBinaryExpr op l r, rst)

    parseExpr :: [Syntax] -> Precedence -> (Expr, [Syntax])
    parseExpr xs@(h:t) p 
      | isUnaryOperator h = parseUnary xs p
      | otherwise = parseBinary xs p

    parseBinary :: [Syntax] -> Precedence -> (Expr, [Syntax])
    parseBinary xs p = 
      case parsePth xs of
        (l, []) -> (l, [])
        (l, ys@(op : tail)) ->
          let precedence = getBinaryOpPrecedence op
            in if precedence > p
                then case parseExpr tail precedence of
                  (r, rst) -> (buildBinaryExpr op l r, rst)
                else (l, ys)

    parseUnary :: [Syntax] -> Precedence -> (Expr, [Syntax])
    parseUnary (operator:tail) p =
      let precedence = getUnaryOpPrecedence operator 
          in if precedence >= p
            then case parseExpr tail precedence of
              (operand, rst) -> (Unary operator operand, rst)
            else error $ "wrong token " <> show operator

    isUnaryOperator :: Syntax -> Bool
    isUnaryOperator Add = True
    isUnaryOperator Sub = True
    isUnaryOperator _ = False

    parsePth :: [Syntax] -> (Expr, [Syntax])
    parsePth (OpenPth : tail) =
      case parseExpr tail 0 of
        (e, ClosePth : rst) -> (Pth e, rst)
    parsePth xs = parseNum xs

    parseNum :: [Syntax] -> (Expr, [Syntax])
    parseNum ((I i) : tail) = (Num i, tail)
    parseNum [] = error "Error empty token"
    parseNum (h : tail) = error $ "Error syntax at " <> show h

    notSpace :: Syntax -> Bool
    notSpace Space = False
    notSpace _ = True

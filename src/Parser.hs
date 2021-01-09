module Parser where

import Lexer

data Expr
  = AddOp Expr Expr
  | MulOp Expr Expr
  | SubOp Expr Expr
  | DivOp Expr Expr
  | Pth Expr
  | Unary Token Expr
  | Num Int
  deriving (Show)

type Precedence = Int

getUnaryOpPrecedence :: Token -> Precedence
getUnaryOpPrecedence Add = 3
getUnaryOpPrecedence Sub = 3
getUnaryOpPrecedence t = 0

getBinaryOpPrecedence :: Token -> Precedence
getBinaryOpPrecedence Mul = 2
getBinaryOpPrecedence Div = 2
getBinaryOpPrecedence Add = 1
getBinaryOpPrecedence Sub = 1
getBinaryOpPrecedence _ = 0

buildBinaryExpr :: Token -> Expr -> Expr -> Expr
buildBinaryExpr Add = AddOp
buildBinaryExpr Sub = SubOp
buildBinaryExpr Mul = MulOp
buildBinaryExpr Div = DivOp

parse :: [Token] -> (Expr, [Token])
parse input = tryRestart $ parseExpr input 0
  where
    tryRestart :: (Expr, [Token]) -> (Expr, [Token])
    tryRestart (l, []) = (l, [])
    tryRestart (l, op : tail) = case parseExpr tail (getBinaryOpPrecedence op) of
      (r, []) -> (buildBinaryExpr op l r, [])
      (r, rst) -> 
        let 
          (rr, rrst) = tryRestart (r, rst)
        in (buildBinaryExpr op l rr, rrst)

    parseExpr :: [Token] -> Precedence -> (Expr, [Token])
    parseExpr xs@(h:t) p 
      | isUnaryOperator h = parseUnary xs p
      | otherwise = parseBinary xs p

    parseBinary :: [Token] -> Precedence -> (Expr, [Token])
    parseBinary xs p = 
      case parsePth xs of
        (l, []) -> (l, [])
        (l, ys@(op : tail)) ->
          let precedence = getBinaryOpPrecedence op
            in if precedence > p
                then case parseExpr tail precedence of
                  (r, rst) -> (buildBinaryExpr op l r, rst)
                else (l, ys)

    parseUnary :: [Token] -> Precedence -> (Expr, [Token])
    parseUnary (operator:tail) p =
      let precedence = getUnaryOpPrecedence operator 
          in if precedence >= p
            then case parseExpr tail precedence of
              (operand, rst) -> (Unary operator operand, rst)
            else error $ "wrong token " <> show operator

    isUnaryOperator :: Token -> Bool
    isUnaryOperator Add = True
    isUnaryOperator Sub = True
    isUnaryOperator _ = False

    parsePth :: [Token] -> (Expr, [Token])
    parsePth (OpenPth : tail) =
      case parseExpr tail 0 of
        (e, ClosePth : rst) -> (Pth e, rst)
    parsePth xs = parseNum xs

    parseNum :: [Token] -> (Expr, [Token])
    parseNum ((I i) : tail) = (Num i, tail)
    parseNum [] = error "Error empty token"
    parseNum (h : tail) = error $ "Error syntax at " <> show h
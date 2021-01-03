module Evaluator (prettyPrint) where

import Parser
import Lexer

eval :: Expr -> Int
eval (Num i) = i
eval (AddOp l r) = eval l + eval r
eval (SubOp l r) = eval l - eval r
eval (MulOp l r) = eval l * eval r
eval (DivOp l r) = eval l `div` eval r
eval (Pth e) = eval e
eval (Unary operator operand) = 
    case operator of
        Add -> eval operand
        Sub -> - (eval operand)

prettyPrint :: Expr -> IO ()
prettyPrint e = do
  print $ eval e
  logST e "" True

logST :: Expr -> String -> Bool -> IO ()
logST (AddOp l r) indent isLast = printBinaryExpr "ADD" indent isLast l r
logST (SubOp l r) indent isLast = printBinaryExpr "SUB" indent isLast l r
logST (MulOp l r) indent isLast = printBinaryExpr "MUL" indent isLast l r
logST (DivOp l r) indent isLast = printBinaryExpr "DIV" indent isLast l r
logST (Pth e) indent isLast = printUnaryExpr "()" indent isLast e 
logST (Unary Add e) indent isLast = printUnaryExpr "ADD" indent isLast e
logST (Unary Sub e) indent isLast = printUnaryExpr "SUB" indent isLast e
logST (Num i) indent _ = do
  putStrLn $ indent <> "└──" <> show i

type Indent = String
type IsLast = Bool
type Symbol = String

printBinaryExpr :: Symbol -> Indent -> IsLast -> Expr -> Expr -> IO ()
printBinaryExpr sym indent isLast l r = 
  let 
    childIndent = indent <> if isLast then "   " else "|  "
    marker = if isLast then "└──" else "├──"
  in do
    putStrLn $ indent <> marker <> sym
    logST l childIndent False
    logST r childIndent True

printUnaryExpr :: Symbol -> Indent -> IsLast -> Expr -> IO ()
printUnaryExpr sym indent isLast e =
  let 
    childIndent = indent <> if isLast then "   " else "|  "
    marker = if isLast then "└──" else "├──"
  in do
    putStrLn $ indent <> marker <> sym
    logST e childIndent True
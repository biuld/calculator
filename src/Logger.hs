module Logger where

import Parser
import Evaluator
import TypeChecker

prettyPrint :: Expr a -> IO ()
prettyPrint e = do
  print $ typeCheck e
  print $ eval e
  logST e "" True
  putStr "\n"

logST :: Expr a -> String -> Bool -> IO ()
logST (Binary op l r) indent isLast = printBinaryExpr (show op) indent isLast l r
logST (Pth e) indent isLast = printUnaryExpr "()" indent isLast e 
logST (Unary op e) indent isLast = printUnaryExpr (show op) indent isLast e
logST (Figure i) indent _ = do
  putStrLn $ indent <> "└──" <> show i
logST (Boolean b) indent _ = do
  putStrLn $ indent <> "└──" <> show b

type Indent = String
type IsLast = Bool
type Symbol = String

printBinaryExpr :: Symbol -> Indent -> IsLast -> Expr a -> Expr a -> IO ()
printBinaryExpr sym indent isLast l r = 
  let 
    childIndent = indent <> if isLast then "   " else "|  "
    marker = if isLast then "└──" else "├──"
  in do
    putStrLn $ indent <> marker <> sym
    logST l childIndent False
    logST r childIndent True

printUnaryExpr :: Symbol -> Indent -> IsLast -> Expr a -> IO ()
printUnaryExpr sym indent isLast e =
  let 
    childIndent = indent <> if isLast then "   " else "|  "
    marker = if isLast then "└──" else "├──"
  in do
    putStrLn $ indent <> marker <> sym
    logST e childIndent True
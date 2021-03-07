module Logger where

import Control.Monad
import Evaluator
import Parser
import TypeChecker

prettyPrint :: Expr -> Bool -> IO ()
prettyPrint e showTree = do
  case (typeCheck e, eval e) of
    (Left msg, _) -> putStrLn msg
    (Right t, v) -> putStrLn $ show v <> " :: " <> show t
  when showTree $ logST e "" True
  putStr "\n"

logST :: Expr -> String -> Bool -> IO ()
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

printBinaryExpr :: Symbol -> Indent -> IsLast -> Expr -> Expr -> IO ()
printBinaryExpr sym indent isLast l r =
  let childIndent = indent <> if isLast then "   " else "|  "
      marker = if isLast then "└──" else "├──"
   in do
        putStrLn $ indent <> marker <> sym
        logST l childIndent False
        logST r childIndent True

printUnaryExpr :: Symbol -> Indent -> IsLast -> Expr -> IO ()
printUnaryExpr sym indent isLast e =
  let childIndent = indent <> if isLast then "   " else "|  "
      marker = if isLast then "└──" else "├──"
   in do
        putStrLn $ indent <> marker <> sym
        logST e childIndent True
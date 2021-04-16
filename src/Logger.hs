module Logger where

import Parser
import Utils
import Data.Foldable

prettyPrint :: Expr -> IO ()
prettyPrint = logST "" True

logST :: String -> Bool -> Expr -> IO ()
logST indent isLast (If b l r) = printBinaryExpr (disp b) indent isLast l r
logST indent isLast (Binary op l r) = printBinaryExpr (disp op) indent isLast l r
logST indent isLast (Pth e) = printUnaryExpr "()" indent isLast e
logST indent isLast (Unary op e) = printUnaryExpr (disp op) indent isLast e
logST indent isLast (Bind n e) = printUnaryExpr n indent isLast e
logST indent isLast (Name n) = putStrLn ""
logST indent _ Unit = do
  putStrLn $ indent <> "└──" <> disp Unit
logST indent _ (Figure i) = do
  putStrLn $ indent <> "└──" <> show i
logST indent _ (Boolean b) = do
  putStrLn $ indent <> "└──" <> show b
logST _ _ (Block es) = traverse_ prettyPrint es

type Indent = String

type IsLast = Bool

type Symbol = String

printBinaryExpr :: Symbol -> Indent -> IsLast -> Expr -> Expr -> IO ()
printBinaryExpr sym indent isLast l r =
  let childIndent = indent <> if isLast then "   " else "|  "
      marker = if isLast then "└──" else "├──"
   in do
        putStrLn $ indent <> marker <> sym
        logST childIndent False l
        logST childIndent True r

printUnaryExpr :: Symbol -> Indent -> IsLast -> Expr -> IO ()
printUnaryExpr sym indent isLast e =
  let childIndent = indent <> if isLast then "   " else "|  "
      marker = if isLast then "└──" else "├──"
   in do
        putStrLn $ indent <> marker <> sym
        logST childIndent True e
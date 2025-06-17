module Language.Calculator.CST.Printer (pprint) where

import Language.Calculator.CST.Types
import Data.Text (unpack)

-- | Tree visualization characters
start :: String
start = "├──"
end :: String
end = "└──"
vertical :: String
vertical = "│  "
space :: String
space = "   "

-- | Print a CST expression with tree visualization
pprintExpr :: String -> Bool -> Expr -> IO ()
pprintExpr prefix isLast expr =
  let (sign, childPrefix) = if isLast
        then (end, prefix <> space)
        else (start, prefix <> vertical)
  in case expr of
    (ExprInt token) -> putStrLn $ prefix <> sign <> show token.tokValue
    (ExprDouble token) -> putStrLn $ prefix <> sign <> show token.tokValue
    (ExprBool token) -> putStrLn $ prefix <> sign <> show token.tokValue
    (ExprString token) -> putStrLn $ prefix <> sign <> show token.tokValue
    (ExprIdent token) -> putStrLn $ prefix <> sign <> unpack token.tokValue
    (ExprTuple es) ->
        putStrLn (prefix <> sign <> "()") >> go childPrefix es
    (ExprUnary op e1) ->
        putStrLn (prefix <> sign <> show op.tokValue) >> pprintExpr childPrefix True e1
    (ExprBinary op e1 e2) ->
        putStrLn (prefix <> sign <> show op.tokValue) >>
        pprintExpr childPrefix False e1 >>
        pprintExpr childPrefix True e2
    (ExprApp ident es) ->
        putStrLn (prefix <> sign <> "APP") >>
        (let identIsLast = null es
             identSign = if identIsLast then end else start
         in putStrLn (childPrefix <> identSign <> unpack ident.tokValue)) >>
        go childPrefix es
    (ExprIf cond thenExpr elseExpr) ->
        putStrLn (prefix <> sign <> "IF") >>
        pprintExpr childPrefix False cond >>
        putStrLn (childPrefix <> start <> "THEN") >>
        pprintExpr (childPrefix <> vertical) True thenExpr >>
        putStrLn (childPrefix <> end <> "ELSE") >>
        pprintExpr (childPrefix <> space) True elseExpr
    (ExprWhile cond body) ->
        putStrLn (prefix <> sign <> "WHILE") >>
        pprintExpr childPrefix False cond >>
        putStrLn (childPrefix <> end <> "DO") >>
        pprintExpr (childPrefix <> space) True body
    (ExprBlock es) ->
        putStrLn (prefix <> sign <> "BLOCK") >> go childPrefix es
    (ExprLet bindings body) ->
        putStrLn (prefix <> sign <> "LET") >>
        mapM_ (\(name, value) -> 
            putStrLn (childPrefix <> start <> unpack name.tokValue) >>
            pprintExpr childPrefix False value) bindings >>
        putStrLn (childPrefix <> start <> "IN") >>
        pprintExpr childPrefix True body
    (ExprLambda params body) ->
        putStrLn (prefix <> sign <> "LAMBDA") >>
        mapM_ (\p -> printParam childPrefix p False) params >>
        putStrLn (childPrefix <> start <> "=>") >>
        pprintExpr childPrefix True body
  where
    go :: String -> [Expr] -> IO ()
    go _ [] = return ()
    go p [h] = pprintExpr p True h
    go p (h : t) = pprintExpr p False h >> go p t

    printParam :: String -> (SourceToken Ident, Expr) -> Bool -> IO()
    printParam p (name, ty) isLastParam = do
      let (paramSign, paramChildPrefix) = if isLastParam
            then (end, p <> space)
            else (start, p <> vertical)
      putStrLn (p <> paramSign <> unpack name.tokValue)
      pprintExpr paramChildPrefix True ty

-- | Print a CST expression starting from the root
pprint :: Expr -> IO ()
pprint expr = pprintExpr "" True expr
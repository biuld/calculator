module Language.Calculator.CST.Printer (pprint) where

import Language.Calculator.CST.Types
import Language.Calculator.Common.Printer
import Data.Text (unpack)
import Language.Calculator.Common.Types (SourceToken (..))

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
    (ExprTuple _ es) ->
        putStrLn (prefix <> sign <> "()") >> go pprintExpr childPrefix es
    (ExprUnary _ op e1) ->
        putStrLn (prefix <> sign <> show op.tokValue) >> pprintExpr childPrefix True e1
    (ExprBinary _ op e1 e2) ->
        putStrLn (prefix <> sign <> show op.tokValue) >>
        pprintExpr childPrefix False e1 >>
        pprintExpr childPrefix True e2
    (ExprApp _ ident es) ->
        putStrLn (prefix <> sign <> "APP") >>
        (let identIsLast = null es
             identSign = if identIsLast then end else start
         in putStrLn (childPrefix <> identSign <> unpack ident.tokValue)) >>
        go pprintExpr childPrefix es
    (ExprIf _ cond thenExpr elseExpr) ->
        putStrLn (prefix <> sign <> "IF") >>
        pprintExpr childPrefix False cond >>
        putStrLn (childPrefix <> start <> "THEN") >>
        pprintExpr (childPrefix <> vertical) True thenExpr >>
        putStrLn (childPrefix <> end <> "ELSE") >>
        pprintExpr (childPrefix <> space) True elseExpr
    (ExprWhile _ cond body) ->
        putStrLn (prefix <> sign <> "WHILE") >>
        pprintExpr childPrefix False cond >>
        putStrLn (childPrefix <> end <> "DO") >>
        pprintExpr (childPrefix <> space) True body
    (ExprBlock _ es) ->
        putStrLn (prefix <> sign <> "BLOCK") >> go pprintExpr childPrefix es
    (ExprLet _ bindings body) -> do
        putStrLn (prefix <> sign <> "LET")
        mapM_ (\(name, value) -> do
            putStrLn (childPrefix <> start <> unpack name.tokValue)
            pprintExpr (childPrefix <> vertical) True value) bindings
        putStrLn (childPrefix <> end <> "IN")
        pprintExpr (childPrefix <> space) True body
    (ExprLambda _ params body) -> do
        putStrLn (prefix <> sign <> "LAMBDA")
        mapM_ (\(name, _) -> putStrLn (childPrefix <> start <> unpack name.tokValue)) params
        putStrLn (childPrefix <> start <> "=>")
        pprintExpr childPrefix True body

-- | Print a CST expression starting from the root
pprint :: Expr -> IO ()
pprint expr = pprintExpr "" True expr
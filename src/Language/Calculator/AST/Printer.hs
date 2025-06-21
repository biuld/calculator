module Language.Calculator.AST.Printer (pprint) where

import Language.Calculator.AST.Types
import Language.Calculator.Common.Printer
import Data.Text (unpack)

pprintExpr :: String -> Bool -> Exists Expr -> IO ()
pprintExpr prefix isLast (Exists e) =
    let
      (sign, childPrefix) = if isLast
        then (end, prefix <> space)
        else (start, prefix <> vertical)
    in case e of
    (ExprLit _ _ val) -> putStrLn $ prefix <> sign <> show val
    (ExprIdent _ i) -> putStrLn $ prefix <> sign <> unpack i
    (ExprTuple _ es) ->
        putStrLn (prefix <> sign <> "()") >> go pprintExpr childPrefix es
    (ExprUnary _ op e1) ->
        putStrLn (prefix <> sign <> show op) >> pprintExpr childPrefix True (Exists e1)
    (ExprBinary _ op e1 e2) ->
        putStrLn (prefix <> sign <> show op) >> 
        pprintExpr childPrefix False (Exists e1) >> 
        pprintExpr childPrefix True (Exists e2)
    (ExprApp _ f a) ->
        putStrLn (prefix <> sign <> "APP") >>
        pprintExpr childPrefix False (Exists f) >>
        pprintExpr childPrefix True (Exists a)
    (ExprIf _ cond thenExpr elseExpr) ->
        putStrLn (prefix <> sign <> "IF") >>
        pprintExpr childPrefix False (Exists cond) >>
        putStrLn (childPrefix <> start <> "THEN") >>
        pprintExpr (childPrefix <> vertical) True (Exists thenExpr) >>
        putStrLn (childPrefix <> end <> "ELSE") >>
        pprintExpr (childPrefix <> space) True (Exists elseExpr)
    (ExprWhile _ cond body) ->
        putStrLn (prefix <> sign <> "WHILE") >>
        pprintExpr childPrefix False (Exists cond) >>
        putStrLn (childPrefix <> end <> "DO") >>
        pprintExpr (childPrefix <> space) True (Exists body)
    (ExprBlock _ es) ->
        putStrLn (prefix <> sign <> "BLOCK") >> go pprintExpr childPrefix es
    ExprUnit ->
        putStrLn $ prefix <> sign <> "UNIT"
    (ExprLet _ bindings body) -> do
        putStrLn (prefix <> sign <> "LET")
        let printBinding (name, value) = do
                putStrLn (childPrefix <> start <> unpack name)
                pprintExpr (childPrefix <> vertical) True value
        mapM_ printBinding bindings
        putStrLn (childPrefix <> end <> "IN")
        pprintExpr (childPrefix <> space) True (Exists body)
    (ExprLambda _ param ty body) ->
        putStrLn (prefix <> sign <> "LAMBDA " <> unpack param <> " : " <> show ty) >>
        pprintExpr childPrefix True (Exists body)

pprint :: Exists Expr -> IO ()
pprint = pprintExpr "" True 
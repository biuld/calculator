module Language.Calculator.AST.Printer (pprint) where

import Language.Calculator.AST.Types
import Data.Text (unpack)

start :: String
start = "├──"
end :: String
end = "└──"
vertical :: String
vertical = "│  "
space :: String
space = "   "

pprintExpr :: String -> Bool -> Exists Expr -> IO ()
pprintExpr prefix isLast (Exists e) =
    let
      (sign, childPrefix) = if isLast
        then (end, prefix <> space)
        else (start, prefix <> vertical)
      go :: String -> [Exists Expr] -> IO ()
      go _ [] = return ()
      go p [h] = pprintExpr p True h
      go p (h : t) = pprintExpr p False h >> go p t
    in case e of
    (ExprLit _ val) -> putStrLn $ prefix <> sign <> show val
    (ExprIdent i) -> putStrLn $ prefix <> sign <> unpack i
    (ExprTuple es) ->
        putStrLn (prefix <> sign <> "()") >> go childPrefix es
    (ExprUnary op e1) ->
        putStrLn (prefix <> sign <> show op) >> pprintExpr childPrefix True (Exists e1)
    (ExprBinary op e1 e2) ->
        putStrLn (prefix <> sign <> show op) >> 
        pprintExpr childPrefix False (Exists e1) >> 
        pprintExpr childPrefix True (Exists e2)
    (ExprApp f a) ->
        putStrLn (prefix <> sign <> "APP") >>
        pprintExpr childPrefix False (Exists f) >>
        pprintExpr childPrefix True (Exists a)
    (ExprIf cond thenExpr elseExpr) ->
        putStrLn (prefix <> sign <> "IF") >>
        pprintExpr childPrefix False (Exists cond) >>
        putStrLn (childPrefix <> start <> "THEN") >>
        pprintExpr (childPrefix <> vertical) True (Exists thenExpr) >>
        putStrLn (childPrefix <> end <> "ELSE") >>
        pprintExpr (childPrefix <> space) True (Exists elseExpr)
    (ExprWhile cond body) ->
        putStrLn (prefix <> sign <> "WHILE") >>
        pprintExpr childPrefix False (Exists cond) >>
        putStrLn (childPrefix <> end <> "DO") >>
        pprintExpr (childPrefix <> space) True (Exists body)
    (ExprBlock es) ->
        putStrLn (prefix <> sign <> "BLOCK") >> go childPrefix es
    ExprUnit ->
        putStrLn $ prefix <> sign <> "UNIT"
    (ExprLet bindings body) -> do
        putStrLn (prefix <> sign <> "LET")
        let printBinding (name, value) = do
                putStrLn (childPrefix <> start <> unpack name)
                pprintExpr (childPrefix <> vertical) True value
        mapM_ printBinding bindings
        putStrLn (childPrefix <> end <> "IN")
        pprintExpr (childPrefix <> space) True (Exists body)
    (ExprLambda param ty body) ->
        putStrLn (prefix <> sign <> "LAMBDA " <> unpack param <> " : " <> show ty) >>
        pprintExpr childPrefix True (Exists body)

pprint :: Exists Expr -> IO ()
pprint = pprintExpr "" True 
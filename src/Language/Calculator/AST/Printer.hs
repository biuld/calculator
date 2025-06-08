module Language.Calculator.AST.Printer (pprint) where

import Language.Calculator.AST.Types
import Data.Text (unpack)

start :: String
start = "├──"
end :: String
end = "└──"

format :: Int -> String -> String -> String
format 0 sign a = sign <> a
format n sign a = "    " <> format (n - 1) sign a

pprintExpr :: Int -> String -> Exists Expr -> IO ()
pprintExpr depth sign (Exists e) = case e of
    (ExprLit _ val) -> putStrLn $ format depth sign (show val)
    (ExprIdent i) -> putStrLn $ format depth sign (unpack i)
    (ExprTuple es) ->
        putStrLn (format depth sign "()") >> go (depth + 1) es
    (ExprUnary op e1) ->
        putStrLn (format depth sign (show op)) >> pprintExpr (depth + 1) end (Exists e1)
    (ExprBinary op e1 e2) ->
        putStrLn (format depth sign (show op)) >> pprintExpr (depth + 1) start (Exists e1) >> pprintExpr (depth + 1) end (Exists e2)
    (ExprApp i es) -> 
        putStrLn (format depth sign "APP") >>  putStrLn (format (depth + 1) start (unpack i)) >> go (depth + 1) es
    (ExprIf cond thenExpr elseExpr) ->
        putStrLn (format depth sign "IF") >>
        pprintExpr (depth + 1) start (Exists cond) >>
        putStrLn (format (depth + 1) start "THEN") >>
        pprintExpr (depth + 2) start (Exists thenExpr) >>
        putStrLn (format (depth + 1) end "ELSE") >>
        pprintExpr (depth + 2) end (Exists elseExpr)
    (ExprWhile cond body) ->
        putStrLn (format depth sign "WHILE") >>
        pprintExpr (depth + 1) start (Exists cond) >>
        putStrLn (format (depth + 1) end "DO") >>
        pprintExpr (depth + 2) end (Exists body)
    (ExprBlock es) ->
        putStrLn (format depth sign "BLOCK") >> go (depth + 1) es
    ExprUnit ->
        putStrLn $ format depth sign "UNIT"
    (ExprLet bindings body) ->
        putStrLn (format depth sign "LET") >>
        mapM_ (\(name, value) -> 
            putStrLn (format (depth + 1) start (unpack name)) >>
            pprintExpr (depth + 2) start value
        ) bindings >>
        putStrLn (format (depth + 1) start "IN") >>
        pprintExpr (depth + 2) end (Exists body)
    (ExprLambda param ty body) ->
        putStrLn (format depth sign "LAMBDA") >>
        putStrLn (format (depth + 1) start (unpack param)) >>
        putStrLn (format (depth + 1) start (show ty)) >>
        pprintExpr (depth + 1) end (Exists body)
  where
    go :: Int -> [Exists Expr] -> IO ()
    go _ [] = return ()
    go d [h] = pprintExpr d end h
    go d (h : t) = pprintExpr d start h >> go d t

pprint :: Exists Expr -> IO ()
pprint = pprintExpr 0 end 
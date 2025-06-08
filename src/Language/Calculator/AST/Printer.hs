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

-- Helper function to wrap expressions with necessary constraints
wrapExpr :: Show (Sing t) => Expr t -> SomeExpr
wrapExpr = SomeExpr

pprintExpr :: Int -> String -> SomeExpr -> IO ()
pprintExpr depth sign (SomeExpr e) = case e of
    (ExprInt i) -> putStrLn $ format depth sign (show i)
    (ExprDouble d) -> putStrLn $ format depth sign (show d)
    (ExprBool b) -> putStrLn $ format depth sign (show b)
    (ExprIdent i) -> putStrLn $ format depth sign (unpack i)
    (ExprString s) -> putStrLn $ format depth sign (unpack s)
    (ExprTuple es) ->
        putStrLn (format depth sign "()") >> go (depth + 1) es
    (ExprUnary op e1) ->
        putStrLn (format depth sign (show op)) >> pprintExpr (depth + 1) end (wrapExpr e1)
    (ExprBinary op e1 e2) ->
        putStrLn (format depth sign (show op)) >> pprintExpr (depth + 1) start (wrapExpr e1) >> pprintExpr (depth + 1) end (wrapExpr e2)
    (ExprApp i es) -> 
        putStrLn (format depth sign "APP") >>  putStrLn (format (depth + 1) start (unpack i)) >> go (depth + 1) es
  where
    go :: Int -> [SomeExpr] -> IO ()
    go _ [] = return ()
    go d [h] = pprintExpr d end h
    go d (h : t) = pprintExpr d start h >> go d t

pprint :: SomeExpr -> IO ()
pprint = pprintExpr 0 end 
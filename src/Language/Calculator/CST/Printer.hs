module Language.Calculator.CST.Printer where

import Language.Calculator.CST.Types

start = "├──"
vb = "│"
end = "└──"

format :: forall a. Show a => Int -> String -> a -> String
format 0 sign a = sign <> show a
format n sign a = "    " <> format (n - 1) sign a

pprintExpr :: Int -> String -> Expr -> IO ()
pprintExpr depth sign e = case e of
    (ExprInt t) -> putStrLn $ format depth sign t.tokValue
    (ExprDouble t) -> putStrLn $ format depth sign t.tokValue
    (ExprBool t) -> putStrLn $ format depth sign t.tokValue
    (ExprIdent t) -> putStrLn $ format depth sign t.tokValue
    (ExprString t) -> putStrLn $ format depth sign t.tokValue
    (ExprTuple es) ->
        putStrLn (format depth sign "()") >> go (depth + 1) es
    (ExprUnary t e1) ->
        putStrLn (format depth sign t.tokValue) >> pprintExpr (depth + 1) sign e1
    (ExprBinary t e1 e2) ->
        putStrLn (format depth sign t.tokValue) >> pprintExpr (depth + 1) start e1 >> pprintExpr (depth + 1) end e2
    (ExprApp t es) -> 
        putStrLn (format depth sign "APP") >>  putStrLn (format (depth + 1) start t.tokValue) >> go (depth + 1) es
  where
    go :: Int -> [Expr] -> IO ()
    go _ [] = return ()
    go d [h] = pprintExpr d end h
    go d (h : t) = pprintExpr d start h >> go d t

pprint :: Expr -> IO ()
pprint = pprintExpr 0 end
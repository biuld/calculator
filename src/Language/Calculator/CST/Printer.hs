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
    (ExprInt (SourceToken _ i)) -> putStrLn $ format depth sign i
    (ExprDouble (SourceToken _ d)) -> putStrLn $ format depth sign d
    (ExprBool (SourceToken _ b)) -> putStrLn $ format depth sign b
    (ExprIdent (SourceToken _ i)) -> putStrLn $ format depth sign i
    (ExprString (SourceToken _ s)) -> putStrLn $ format depth sign s
    (ExprTuple es) ->
        putStrLn (format depth sign "()") >> go (depth + 1) es
    (ExprUnary (SourceToken _ op) e1) ->
        putStrLn (format depth sign op) >> pprintExpr (depth + 1) sign e1
    (ExprBinary (SourceToken _ op) e1 e2) ->
        putStrLn (format depth sign op) >> pprintExpr (depth + 1) start e1 >> pprintExpr (depth + 1) end e2
    (ExprBind (SourceToken _ i) e1) ->
        putStrLn (format depth sign "ABS") >> putStrLn (format (depth + 1) sign i) >> pprintExpr (depth + 1) sign e1
    (ExprApp (SourceToken _ i) es) -> 
        putStrLn (format depth sign "APP") >>  putStrLn (format (depth + 1) start i) >> go (depth + 1) es
  where
    go :: Int -> [Expr] -> IO ()
    go _ [] = return ()
    go d [h] = pprintExpr d end h
    go d (h : t) = pprintExpr d start h >> go d t

pprint :: Expr -> IO ()
pprint = pprintExpr 0 end
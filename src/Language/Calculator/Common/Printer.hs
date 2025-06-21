module Language.Calculator.Common.Printer (
    start,
    end,
    vertical,
    space,
    go
) where

-- import Language.Calculator.AST.Types (Exists(..), Expr(..)) -- Removed
-- import Language.Calculator.CST.Types (Expr(..)) -- Removed

-- | Tree visualization characters
start :: String
start = "├──"
end :: String
end = "└──"
vertical :: String
vertical = "│  "
space :: String
space = "   "

-- | Helper function for printing lists of expressions (polymorphic)
go :: (String -> Bool -> a -> IO ()) -> String -> [a] -> IO ()
go _ _ [] = return ()
go pprintFn p [h] = pprintFn p True h
go pprintFn p (h : t) = pprintFn p False h >> go pprintFn p t 
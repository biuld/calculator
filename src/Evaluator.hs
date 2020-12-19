module Evaluator where

import Parser

eval :: Expr -> Int
eval (Num i) = i
eval (AddOp l r) = eval l + eval r
eval (SubOp l r) = eval l - eval r
eval (MulOp l r) = eval l * eval r
eval (DivOp l r) = eval l `div` eval r
eval (Pth e) = eval e
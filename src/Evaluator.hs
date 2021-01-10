module Evaluator where

import Parser
import Lexer

eval :: Expr a -> Expr a
eval (Figure i) = Figure i
eval (Boolean b) = Boolean b
eval (Pth e) = eval e
eval (Binary op l r) = 
  case (op, eval l, eval r) of
    (Add, Figure li, Figure ri) -> Figure $ li + ri
    (Sub, Figure li, Figure ri) -> Figure $ li - ri
    (Mul, Figure li, Figure ri) -> Figure $ li * ri
    (Div, Figure li, Figure ri) -> Figure $ li `div` ri
    (Equal, Figure li, Figure ri) -> Boolean $ li == ri
    (Equal, Boolean lb, Boolean rb) -> Boolean $ lb == rb
    (And, Boolean lb, Boolean rb) -> Boolean $ lb && rb
    (Or, Boolean lb, Boolean rb) -> Boolean $ lb || rb
eval (Unary op e) = 
  case (op, eval e) of
    (Add, Figure i) -> Figure i
    (Sub, Figure i) -> Figure $ -i
    (Not, Boolean b) -> Boolean $ not b
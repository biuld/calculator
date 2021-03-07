module Evaluator where

import Lexer
import Parser

eval :: Expr -> Expr
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
    (Equal, Figure _, Boolean _) -> Boolean False
    (Equal, Boolean _, Figure _) -> Boolean False
    (NotEqual, Figure li, Figure ri) -> Boolean $ li /= ri
    (NotEqual, Boolean lb, Boolean rb) -> Boolean $ lb /= rb
    (NotEqual, Figure _, Boolean _) -> Boolean True
    (NotEqual, Boolean _, Figure _) -> Boolean True
    (And, Boolean lb, Boolean rb) -> Boolean $ lb && rb
    (Or, Boolean lb, Boolean rb) -> Boolean $ lb || rb
eval (Unary op e) =
  case (op, eval e) of
    (Add, Figure i) -> Figure i
    (Sub, Figure i) -> Figure $ - i
    (Not, Boolean b) -> Boolean $ not b
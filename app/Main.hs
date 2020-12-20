module Main where

import Evaluator (eval)
import Lexer
import Parser

main :: IO [()]
main =
  traverse
    (printRes . parse . lexx)
    [ "1 + 2 + 3 + 4", --10
      "(1 + 2) * 3 - 4", --5
      "1 * 2 * 3 * 4" --24
    ]
  where
    printRes :: (Expr, [Syntax]) -> IO ()
    printRes (e, t) = do
      print e
      print t
      print . eval $ e

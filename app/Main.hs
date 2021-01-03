module Main where

import Evaluator
import Lexer
import Parser

main :: IO [()]
main =
  traverse
    (prettyPrint . fst . parse . lexx)
    [ 
      "-1 + (1 - 1) * 1 / 1",
      "(1 + 2) * 3 - 4",
      "1 * 2 * 3 * 4",
      "+ - - + 3" 
    ]

module Main where

import Evaluator
import Lexer
import Parser
import Logger

main :: IO [()]
main =
  traverse
    (prettyPrint . fst . parse . lexx)
    [ 
      "-1 + (1 - 1) * 1 / 1",
      "(1 + 2) * 3 - 4",
      "1 * 2 * 3 * 4",
      "+ - - + 3", 
      " 1 eq 1",
      "(1+2) eq 3",
      "not true",
      "true and false",
      "false or false",
      "true eq false" 
    ]

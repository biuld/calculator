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
      " 1 == 1",
      "(1+2) == 3",
      "! true",
      "true && false",
      "false || false",
      "true == false",
      "1 == 2 && 2 == 3",
      "1 == 2 && 2 == 2",
      "true && true == true || false",
      "1 != 2",
      "true != false",
      "1 != true"
    ]

module Main where

import Evaluator (eval)
import Lexer (lexx)
import Parser (parse)

main :: IO ()
main =
  let expr = fst . parse . lexx $ "12 + (2 - 3) * 9 / 2"
   in case expr of
        Just e -> do
          print e
          print . eval $ e

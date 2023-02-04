module Language.Calculator.CST where

import Data.Text
import Data.Void
import Language.Calculator.CST.Parser (stm)
import Language.Calculator.CST.Types
import Text.Megaparsec qualified as M

parse :: String -> Text -> Either (M.ParseErrorBundle Text Void) Statement
parse = M.runParser stm

parse' = parse ""
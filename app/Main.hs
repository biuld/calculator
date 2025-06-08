module Main (module Main) where

import Data.Text (pack)
import qualified Data.Map as Map
import Language.Calculator.CST.Parser (parseExpr)
import Language.Calculator.Desugar (desugar, TypedExpr(..))
import Language.Calculator.AST.Printer (pprint)
import Language.Calculator.AST.Types (SomeExpr(..))
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = loop Map.empty
  where
    loop typeEnv = do
      putStr "> "
      hFlush stdout
      input <- getLine
      case input of
        ":q" -> pure ()
        _ -> do
          case parseExpr (pack input) of
            Left err -> putStrLn $ errorBundlePretty err
            Right cst -> do
              case desugar typeEnv cst of
                Left err -> print err
                Right (TypedExpr _ ast) -> pprint (SomeExpr ast)
              loop typeEnv
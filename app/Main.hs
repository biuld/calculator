module Main (module Main) where

import Data.Text (pack)
import qualified Data.Map as Map
import Language.Calculator.CST.Parser (parseExpr)
import Language.Calculator.Desugar (desugar, TypedExpr(..))
import Language.Calculator.AST.Printer (pprint)
import Language.Calculator.AST.Types (SomeExpr(..))
import Language.Calculator.AST.Interpreter (interpret)
import Language.Calculator.AST.Env (Env, emptyEnv)
import System.IO (hFlush, stdout)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = loop Map.empty emptyEnv
  where
    loop typeEnv env = do
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
                Right (TypedExpr _ ast) -> do
                  putStrLn "AST:"
                  pprint (SomeExpr ast)
                  putStrLn "Result:"
                  pprint (SomeExpr (interpret env ast))
              loop typeEnv env
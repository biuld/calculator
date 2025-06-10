module Main (module Main) where

import Data.Text (pack)
import qualified Data.Map as Map
import Language.Calculator.CST.Parser (parseExpr)
import qualified Language.Calculator.CST.Printer as CSTPrinter
import Language.Calculator.Desugar (desugar)
import qualified Language.Calculator.AST.Printer as ASTPrinter
import Language.Calculator.AST.Types (TypeExpr(..), Exists(..), Expr(..))
import Language.Calculator.AST.Interpreter (interpret)
import Language.Calculator.AST.Env (emptyEnv)
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty)

-- | Handle parse command
handleParse :: Bool -> String -> IO ()
handleParse raw expr = do
  case parseExpr (pack expr) of
    Left err -> putStrLn $ errorBundlePretty err
    Right cst -> do
      if raw
        then print cst
        else do
          putStrLn "CST:"
          CSTPrinter.pprint cst

-- | Handle desugar command
handleDesugar :: Bool -> String -> IO ()
handleDesugar raw expr = do
  case parseExpr (pack expr) of
    Left err -> putStrLn $ errorBundlePretty err
    Right cst -> do
      case desugar Map.empty cst of
        Left err -> print err
        Right (TypeExpr _ ast) -> 
          if raw
            then print ast
            else do
              putStrLn "AST:"
              ASTPrinter.pprint (Exists ast :: Exists Expr)

-- | Handle eval command
handleEval :: Bool -> String -> IO ()
handleEval raw expr = do
  case parseExpr (pack expr) of
    Left err -> putStrLn $ errorBundlePretty err
    Right cst -> do
      case desugar Map.empty cst of
        Left err -> print err
        Right (TypeExpr _ ast) -> do
          let result = interpret emptyEnv ast
          if raw
            then print result
            else ASTPrinter.pprint (Exists result :: Exists Expr)

-- | Print usage information
printUsage :: IO ()
printUsage = do
  putStrLn "Usage: calculator <command> [-r] <expression>"
  putStrLn "Commands:"
  putStrLn "  parse   - Parse the expression and show CST"
  putStrLn "  desugar - Parse and desugar the expression"
  putStrLn "  eval    - Evaluate the expression"
  putStrLn "Options:"
  putStrLn "  -r      - Show raw output using print"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printUsage
    [_] -> printUsage
    (cmd:rest) -> do
      let (raw, e) = case rest of
            "-r":expr:_ -> (True, expr)
            expr:_ -> (False, expr)
      case cmd of
        "parse" -> handleParse raw e
        "desugar" -> handleDesugar raw e
        "eval" -> handleEval raw e
        _ -> printUsage
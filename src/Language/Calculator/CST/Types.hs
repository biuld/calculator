module Language.Calculator.CST.Types (
  Operator (..),
  Expr (..),
  Ident,
  keywords,
  opToText,
  getExprRange,
) where

import Data.Text
-- import Text.Megaparsec -- Removed redundant import
import Language.Calculator.Common.Types (SourceToken(..), SourceRange(..))

data Operator
  = OpPlus
  | OpMinus
  | OpMultiply
  | OpDivide
  | OpEqual
  | OpNotEqual
  | OpAnd
  | OpOr
  | OpNot
  deriving (Eq, Show)

opToText :: Operator -> Text
opToText OpPlus = "+"
opToText OpMinus = "-"
opToText OpMultiply = "*"
opToText OpDivide = "/"
opToText OpEqual = "=="
opToText OpNotEqual = "!="
opToText OpAnd = "&&"
opToText OpOr = "||"
opToText OpNot = "!"

type Ident = Text

data Expr
  = ExprInt (SourceToken Integer)
  | ExprDouble (SourceToken Double)
  | ExprBool (SourceToken Bool)
  | ExprString (SourceToken Text)
  | ExprIdent (SourceToken Ident)
  | ExprTuple (SourceToken ()) [Expr]
  | ExprUnary (SourceToken ()) (SourceToken Operator) Expr
  | ExprBinary (SourceToken ()) (SourceToken Operator) Expr Expr
  | ExprApp (SourceToken ()) (SourceToken Ident) [Expr]
  | ExprIf (SourceToken ()) Expr Expr Expr
  | ExprWhile (SourceToken ()) Expr Expr
  | ExprBlock (SourceToken ()) [Expr]
  | ExprLet (SourceToken ()) [(SourceToken Ident, Expr)] Expr
  | ExprLambda (SourceToken ()) [(SourceToken Ident, Expr)] Expr
  deriving (Eq, Show)

getExprRange :: Expr -> SourceRange
getExprRange (ExprInt (SourceToken r _)) = r
getExprRange (ExprDouble (SourceToken r _)) = r
getExprRange (ExprBool (SourceToken r _)) = r
getExprRange (ExprString (SourceToken r _)) = r
getExprRange (ExprIdent (SourceToken r _)) = r
getExprRange (ExprTuple (SourceToken r _) _) = r
getExprRange (ExprUnary (SourceToken r _) _ _) = r
getExprRange (ExprBinary (SourceToken r _) _ _ _) = r
getExprRange (ExprApp (SourceToken r _) _ _) = r
getExprRange (ExprIf (SourceToken r _) _ _ _) = r
getExprRange (ExprWhile (SourceToken r _) _ _) = r
getExprRange (ExprBlock (SourceToken r _) _) = r
getExprRange (ExprLet (SourceToken r _) _ _) = r
getExprRange (ExprLambda (SourceToken r _) _ _) = r

keywords :: [String]
keywords = ["if", "else", "let", "for", "while", "function"]
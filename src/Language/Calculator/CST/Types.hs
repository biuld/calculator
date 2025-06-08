module Language.Calculator.CST.Types (
  Operator (..),
  Expr (..),
  Ident,
  SourceToken (..),
  SourceRange (..),
  keywords,
  opToText,
) where

import Data.Text
import Text.Megaparsec

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
  | ExprTuple [Expr]
  | ExprUnary (SourceToken Operator) Expr
  | ExprBinary (SourceToken Operator) Expr Expr
  | ExprApp (SourceToken Ident) [Expr]
  | ExprIf Expr Expr Expr  -- if condition thenExpr elseExpr
  | ExprWhile Expr Expr   -- while condition body
  | ExprBlock [Expr]      -- Block of expressions
  | ExprLet (SourceToken Ident) Expr Expr  -- let name = value in body
  deriving (Eq, Show)

data SourceRange = SourceRange
  { srcStart :: SourcePos
  , srcEnd :: SourcePos
  }
  deriving (Eq)

instance Show SourceRange where
  show (SourceRange s e) = sourcePosPretty s <> " - " <> sourcePosPretty e

data SourceToken a = SourceToken
  { tokRange :: SourceRange
  , tokValue :: a
  }
  deriving (Eq, Show)

keywords :: [String]
keywords = ["if", "else", "let", "for", "while", "function"]
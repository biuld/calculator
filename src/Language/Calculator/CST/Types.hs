module Language.Calculator.CST.Types (
  Operator (..),
  Expr (..),
  Ident,
  SourceToken (..),
  SourceRange (..),
  getOpPrecedence,
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

getOpPrecedence :: Operator -> Int
getOpPrecedence OpPlus = 1
getOpPrecedence OpMinus = 1
getOpPrecedence OpMultiply = 2
getOpPrecedence OpDivide = 2
getOpPrecedence OpEqual = 0
getOpPrecedence OpNotEqual = 0
getOpPrecedence OpAnd = 3
getOpPrecedence OpOr = 3
getOpPrecedence OpNot = 4

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
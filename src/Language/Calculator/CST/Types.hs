module Language.Calculator.CST.Types (
  Operator (..),
  Expr (..),
  Ident,
  Statement (..),
  SourceToken (..),
  SourceRange (..),
  ge,
  keywords,
) where

import Data.Text
import Text.Megaparsec

-- data Operator
--   = Add
--   | Mul
--   | Div
--   | Sub
--   | Equal
--   | NotEqual
--   | And
--   | Or
--   | Not
--   deriving (Eq, Show)

getOpPrecedence :: Operator -> OperatorPrecedence
getOpPrecedence "+" = 1
getOpPrecedence "-" = 1
getOpPrecedence "/" = 2
getOpPrecedence "*" = 2
getOpPrecedence "==" = 0
getOpPrecedence "!=" = 0
getOpPrecedence "&&" = 3
getOpPrecedence "||" = 3
getOpPrecedence "!" = 4
getOpPrecedence _ = error "undefined operator"

ge :: Operator -> Operator -> Bool
ge op1 op2 = getOpPrecedence op1 >= getOpPrecedence op2

type Ident = Text

type Operator = Text

type OperatorPrecedence = Int

data Expr
  = ExprInt (SourceToken Integer)
  | ExprDouble (SourceToken Double)
  | ExprBool (SourceToken Bool)
  | ExprString (SourceToken Text)
  | ExprIdent (SourceToken Ident)
  | ExprTuple [Expr]
  | ExprUnary (SourceToken Operator) Expr
  | ExprBinary (SourceToken Operator) Expr Expr
  | ExprBind (SourceToken Ident) Expr
  | ExprApp Expr [Expr]
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

data Statement
  = StmAbs (SourceToken Ident) [Expr] [Statement]
  | StmIfElse Expr [Statement] [Statement]
  | StmWhile Expr [Statement]
  | StmFor Expr Expr Expr [Statement]
  | StmE Expr
  deriving (Eq, Show)

keywords :: [String]
keywords = ["if", "else", "let", "for", "while"]
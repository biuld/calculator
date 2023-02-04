module Language.Calculator.CST.Types where

import Data.Text

data Operator
  = Add
  | Mul
  | Div
  | Sub
  | Equal
  | NotEqual
  | And
  | Or
  | Not
  deriving (Eq, Show)

instance Ord Operator where
  Add <= Sub = True
  Sub <= Add = True
  Add <= Mul = True
  Div <= Mul = True
  Mul <= Div = True

  Mul <= And = True

  And <= Or = True
  Or <= And = True
  And <= Not = True

  Equal <= NotEqual = True
  NotEqual <= Equal = True
  Equal <= Add = True
  Equal <= And = True

type Ident = Text

data Expr
  = ExprInt Integer
  | ExprDouble Double
  | ExprBool Bool
  | ExprString Text
  | ExprTuple [Expr]
  | ExprUnary Operator Expr
  | ExprBinary Operator Expr Expr
  | ExprBind Ident Expr
  | ExprIdent Ident
  | ExprApp Expr [Expr]
  deriving (Eq, Show)

data Statement
  = StmAbs Ident [Expr] [Statement]
  | StmIfElse Expr [Statement] [Statement]
  | StmWhile Expr [Statement]
  | StmFor Expr Expr Expr [Statement]
  | StmE Expr
  deriving (Eq, Show)
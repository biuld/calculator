module CST where

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

type Ident = Text

data Expr
  = ExprInt Integer
  | ExprDouble Double
  | ExprBool Bool
  | ExprWrapped [Expr]
  | ExprUnary Operator Expr
  | ExprBinary Operator Expr Expr
  | ExprBind Ident Expr
  | ExprIdent Ident
  deriving (Eq, Show)

data Statement
  = StmAbs Ident Expr [Statement]
  | StmApp Expr Expr
  | StmIfElse Expr [Statement] [Statement]
  | StmWhile Expr [Statement]
  | StmFor Expr Expr Expr [Statement]
  | StmE Expr
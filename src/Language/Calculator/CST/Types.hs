module Language.Calculator.CST.Types (
  Operator(..),
  Expr(..),
  Ident
  ,Statement(..)
  , ge
) where

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

getOpPrecedence :: Operator -> Int
getOpPrecedence Add = 1
getOpPrecedence Sub = 1
getOpPrecedence Div = 2
getOpPrecedence Mul = 2
getOpPrecedence Equal = 0
getOpPrecedence NotEqual = 0
getOpPrecedence And = 3
getOpPrecedence Or = 3
getOpPrecedence Not = 4

ge :: Operator -> Operator -> Bool
ge op1 op2 = getOpPrecedence op1 >= getOpPrecedence op2


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
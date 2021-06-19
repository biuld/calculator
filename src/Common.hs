{-# LANGUAGE TemplateHaskell #-}

module Common where

import Control.Monad.Except (ExceptT)
import Control.Monad.State.Strict (MonadState (get, put), State)
import Data.List (intercalate)
import Data.Map.Strict (Map, empty)
import Optics (makeLenses, (&), (.~))

data Token
  = I Int
  | B Bool
  | N String
  | Add
  | Mul
  | Div
  | Sub
  | Equal
  | NotEqual
  | And
  | Or
  | Not
  | Space
  | OpenPth
  | ClosePth
  | Ift
  | Elt
  | Assign
  | Let
  | CommaSep
  | LineSep
  | Def
  | OpenBracket
  | CloseBracket
  deriving (Eq, Show)

type Precedence = Int

unOps = [Add, Sub, Not]

getUnaryOpPrecedence :: Token -> Precedence
getUnaryOpPrecedence Add = 6
getUnaryOpPrecedence Sub = 6
getUnaryOpPrecedence Not = 6
getUnaryOpPrecedence _ = 0

binOps = [Add, Sub, Mul, Div, And, Or, Equal, NotEqual]

getBinaryOpPrecedence :: Token -> Precedence
getBinaryOpPrecedence Mul = 5
getBinaryOpPrecedence Div = 5
getBinaryOpPrecedence Add = 4
getBinaryOpPrecedence Sub = 4
getBinaryOpPrecedence Equal = 3
getBinaryOpPrecedence NotEqual = 3
getBinaryOpPrecedence And = 2
getBinaryOpPrecedence Or = 1
getBinaryOpPrecedence _ = 0

data Expr
  = Figure Int
  | Boolean Bool
  | Unit
  | Pth Expr
  | Unary Token Expr
  | Binary Token Expr Expr
  | If Expr Expr Expr
  | Bind String Expr
  | Name String
  | Group [Expr]
  | FuncDef String [String] [Expr]
  | FuncCall String [Expr]
  deriving (Eq, Show)

class Show a => Display a where
  disp :: a -> String
  disp a = show a

instance Display Token where
  disp (I i) = show i
  disp (B b) = show b
  disp (N n) = n
  disp Add = "+"
  disp Sub = "-"
  disp Mul = "*"
  disp Div = "/"
  disp Equal = "=="
  disp NotEqual = "!="
  disp And = "&&"
  disp Or = "||"
  disp Not = "!"
  disp OpenPth = "("
  disp ClosePth = ")"
  disp Space = " "
  disp Ift = "if"
  disp Elt = "else"
  disp Assign = "="
  disp Let = "let"
  disp CommaSep = ","
  disp LineSep = ";"
  disp Def = "def"
  disp OpenBracket = "{"
  disp CloseBracket = "}"

instance Display Expr where
  disp (Figure i) = show i <> " :: Figure"
  disp (Boolean b) = show b <> " :: Boolean"
  disp (Pth e) = "( " <> disp e <> " )"
  disp (Binary op l r) = disp l <> " " <> disp op <> " " <> disp r
  disp (Unary op e) = disp op <> " " <> disp e
  disp Unit = "()"
  disp (Name t) = t
  disp (Group es) = "(" <> intercalate ", " (fmap disp es) <> ")"
  disp other = show other

data Context = Context
  { _tokens :: [Token],
    _names :: Map String Expr,
    _tree :: Expr,
    _value :: Expr,
    _parent :: Maybe Context
  }
  deriving (Eq, Show)

makeLenses ''Context

type Pack a b = ExceptT String (State a) b

restore :: [Token] -> Pack Context ()
restore t = do
  c <- get
  put (c & tokens .~ t)

emptyContext :: Context
emptyContext = Context [] Data.Map.Strict.empty Unit Unit Nothing
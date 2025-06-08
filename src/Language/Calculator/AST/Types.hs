{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Types (
    T(..),
    STy(..),
    fromSTy,
    TypeEnv,
    TypeError(..),
    Sing,
    SomeExpr(..),
    Expr(..),
    Op(..)
) where

import Data.Text
import qualified Data.Map as Map
import Data.Type.Equality

data T
  = TInt
  | TDouble
  | TBool
  | TString
  | TTuple
  | TUnit
  deriving (Show, Eq)

data STy (t :: T) where
  SInt :: STy 'TInt
  SDouble :: STy 'TDouble
  SBool :: STy 'TBool
  SString :: STy 'TString
  STuple :: STy 'TTuple
  SUnit :: STy 'TUnit

deriving instance Show (STy t)

instance TestEquality STy where
  testEquality SInt SInt = Just Refl
  testEquality SDouble SDouble = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality SString SString = Just Refl
  testEquality STuple STuple = Just Refl
  testEquality SUnit SUnit = Just Refl
  testEquality _ _ = Nothing

fromSTy :: STy t -> T
fromSTy SInt = TInt
fromSTy SDouble = TDouble
fromSTy SBool = TBool
fromSTy SString = TString
fromSTy STuple = TTuple
fromSTy SUnit = TUnit

-- Type environment, for now just for variables.
type TypeEnv = Map.Map Text T

data TypeError =
    UnboundVariable Text
  | TypeMismatch T T
  | OpMismatch Text T
  | ArgNumMismatch Int Int
  | NotAFunction Text
  | OtherError String
  deriving (Show, Eq)

type family Sing (t :: T) where
  Sing 'TInt = Integer
  Sing 'TDouble = Double
  Sing 'TBool = Bool
  Sing 'TString = Text
  Sing 'TTuple = () -- Simplified for now
  Sing 'TUnit = ()

data SomeExpr = forall t. Show (Sing t) => SomeExpr (Expr t)

instance Show SomeExpr where
  show (SomeExpr e) = show e

data Expr t where
  ExprInt :: Integer -> Expr 'TInt
  ExprDouble :: Double -> Expr 'TDouble
  ExprBool :: Bool -> Expr 'TBool
  ExprString :: Text -> Expr 'TString
  ExprIdent :: Text -> Expr t -- Type is unknown at this stage, will be resolved in type checking
  ExprTuple :: [SomeExpr] -> Expr 'TTuple
  ExprUnary :: (Show (Sing input), Show (Sing output)) => Op input output -> Expr input -> Expr output
  ExprBinary :: (Show (Sing input), Show (Sing output)) => Op input output -> Expr input -> Expr input -> Expr output
  ExprApp :: Text -> [SomeExpr] -> Expr t -- Return type is also unknown for now

instance Show (Expr t) where
  show (ExprInt i) = "ExprInt " <> show i
  show (ExprDouble d) = "ExprDouble " <> show d
  show (ExprBool b) = "ExprBool " <> show b
  show (ExprString s) = "ExprString " <> show s
  show (ExprIdent i) = "ExprIdent " <> unpack i
  show (ExprTuple es) = "ExprTuple " <> show es
  show (ExprUnary op e) = "ExprUnary " <> show op <> " (" <> show e <> ")"
  show (ExprBinary op e1 e2) = "ExprBinary " <> show op <> " (" <> show e1 <> ") (" <> show e2 <> ")"
  show (ExprApp f args) = "ExprApp " <> unpack f <> " " <> show args

data Op (input :: T) (output :: T) where
  -- Arithmetic
  OpPlus :: Op 'TDouble 'TDouble
  OpMinus :: Op 'TDouble 'TDouble
  OpMultiply :: Op 'TDouble 'TDouble
  OpDivide :: Op 'TDouble 'TDouble
  OpPlusInt :: Op 'TInt 'TInt
  OpMinusInt :: Op 'TInt 'TInt
  OpMultiplyInt :: Op 'TInt 'TInt
  OpDivideInt :: Op 'TInt 'TInt
  -- Logical
  OpAnd :: Op 'TBool 'TBool
  OpOr :: Op 'TBool 'TBool
  OpNot :: Op 'TBool 'TBool
  -- Equality (example for Int)
  OpEqualInt :: Op 'TInt 'TBool
  OpNotEqualInt :: Op 'TInt 'TBool
  -- Equality for Double
  OpEqualDouble :: Op 'TDouble 'TBool
  OpNotEqualDouble :: Op 'TDouble 'TBool
  -- Equality for Bool
  OpEqualBool :: Op 'TBool 'TBool
  OpNotEqualBool :: Op 'TBool 'TBool
  -- Equality for String
  OpEqualString :: Op 'TString 'TBool
  OpNotEqualString :: Op 'TString 'TBool

deriving instance Show (Op input output) 
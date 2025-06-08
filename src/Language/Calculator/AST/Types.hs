{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Types (
    TermT(..),
    Exists(..),
    TypeError(..),
    RealType,
    Expr(..),
    Op(..),
    TypeExpr(..),
    TypeEnv,
    T(..)
) where

import Data.Text
import qualified Data.Map as Map
import Data.Type.Equality
import Data.Kind (Type)

-- Basic type definitions
data T
  = TInt
  | TDouble
  | TBool
  | TString
  | TTuple
  | TUnit

-- TermT is a singleton type that associates type-level T with term-level values
-- It allows us to work with type information at runtime
data TermT (t :: T) where
  SInt :: TermT 'TInt
  SDouble :: TermT 'TDouble
  SBool :: TermT 'TBool
  SString :: TermT 'TString
  STuple :: TermT 'TTuple
  SUnit :: TermT 'TUnit

deriving instance Show (TermT t)

instance TestEquality TermT where
  testEquality SInt SInt = Just Refl
  testEquality SDouble SDouble = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality SString SString = Just Refl
  testEquality STuple STuple = Just Refl
  testEquality SUnit SUnit = Just Refl
  testEquality _ _ = Nothing

-- Existential type wrapper
data Exists (f :: k -> Type) where
  Exists :: f a -> Exists f

instance Show (Exists TermT) where
  show (Exists SInt) = "Int"
  show (Exists SDouble) = "Double"
  show (Exists SBool) = "Bool"
  show (Exists SString) = "String"
  show (Exists STuple) = "Tuple"
  show (Exists SUnit) = "Unit"

instance Eq (Exists TermT) where
  Exists a == Exists b = case testEquality a b of
    Just Refl -> True
    Nothing -> False

-- Type error definitions
data TypeError where
  UnboundVariable :: Text -> TypeError
  TypeMismatch :: Exists TermT -> Exists TermT -> TypeError
  OpMismatch :: Text -> Exists TermT -> TypeError
  ArgNumMismatch :: Int -> Int -> TypeError
  NotAFunction :: Text -> TypeError
  OtherError :: String -> TypeError
  deriving (Show, Eq)

-- Type family definition, mapping type-level T to concrete runtime types
type family RealType (t :: T) where
  RealType 'TInt = Integer
  RealType 'TDouble = Double
  RealType 'TBool = Bool
  RealType 'TString = Text
  RealType 'TTuple = () -- Simplified for now
  RealType 'TUnit = ()

-- Typed expressions
-- Type-safe expressions
data Expr t where
  ExprInt :: RealType 'TInt -> Expr 'TInt
  ExprDouble :: RealType 'TDouble -> Expr 'TDouble
  ExprBool :: RealType 'TBool -> Expr 'TBool
  ExprString :: RealType 'TString -> Expr 'TString
  ExprIdent :: Text -> Expr t
  ExprTuple :: [Exists Expr] -> Expr 'TTuple
  ExprUnary :: (Show (RealType input), Show (RealType output)) => Op input output -> Expr input -> Expr output
  ExprBinary :: (Show (RealType input), Show (RealType output)) => Op input output -> Expr input -> Expr input -> Expr output
  ExprApp :: Text -> [Exists Expr] -> Expr t
  ExprIf :: Expr 'TBool -> Expr t -> Expr t -> Expr t
  ExprWhile :: Expr 'TBool -> Expr 'TUnit -> Expr 'TUnit
  ExprBlock :: [Exists Expr] -> Expr 'TUnit
  ExprUnit :: Expr 'TUnit
  ExprLet :: Text -> Exists Expr -> Expr t -> Expr t  -- Let binding: let x = e1 in e2

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
  show (ExprIf cond thenExpr elseExpr) = "ExprIf (" <> show cond <> ") (" <> show thenExpr <> ") (" <> show elseExpr <> ")"
  show (ExprWhile cond body) = "ExprWhile (" <> show cond <> ") (" <> show body <> ")"
  show (ExprBlock es) = "ExprBlock " <> show es
  show ExprUnit = "ExprUnit"
  show (ExprLet name value body) = "ExprLet " <> unpack name <> " (" <> show value <> ") (" <> show body <> ")"

-- Type-safe operators
data Op (input :: T) (output :: T) where
  -- Binary operations for Double
  OpDoubleAdd :: Op 'TDouble 'TDouble
  OpDoubleSub :: Op 'TDouble 'TDouble
  OpDoubleMul :: Op 'TDouble 'TDouble
  OpDoubleDiv :: Op 'TDouble 'TDouble
  
  -- Binary operations for Int
  OpIntAdd :: Op 'TInt 'TInt
  OpIntSub :: Op 'TInt 'TInt
  OpIntMul :: Op 'TInt 'TInt
  OpIntDiv :: Op 'TInt 'TInt
  
  -- Unary operations for Double
  OpDoublePos :: Op 'TDouble 'TDouble
  OpDoubleNeg :: Op 'TDouble 'TDouble
  
  -- Unary operations for Int
  OpIntPos :: Op 'TInt 'TInt
  OpIntNeg :: Op 'TInt 'TInt
  
  -- Logical operations
  OpAnd :: Op 'TBool 'TBool
  OpOr :: Op 'TBool 'TBool
  OpNot :: Op 'TBool 'TBool
  
  -- Equality operations
  OpIntEq :: Op 'TInt 'TBool
  OpIntNe :: Op 'TInt 'TBool
  OpDoubleEq :: Op 'TDouble 'TBool
  OpDoubleNe :: Op 'TDouble 'TBool
  OpBoolEq :: Op 'TBool 'TBool
  OpBoolNe :: Op 'TBool 'TBool
  OpStringEq :: Op 'TString 'TBool
  OpStringNe :: Op 'TString 'TBool

deriving instance Show (Op input output)

-- Type-safe value wrapper
data TypeExpr where
  TypeExpr :: forall t. TermT t -> Expr t -> TypeExpr

instance Show TypeExpr where
  show (TypeExpr ty expr) = "TypeExpr " <> show ty <> " " <> show expr

-- Type environment
type TypeEnv = Map.Map Text TypeExpr 

-- Add Show instance for Exists (Expr t)
instance Show (Exists (Expr :: T -> Type)) where
  show (Exists e) = show e 
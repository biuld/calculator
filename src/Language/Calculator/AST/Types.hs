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
  | TFun T T  -- Function type: input -> output (single parameter)

-- TermT is a singleton type that associates type-level T with term-level values
-- It allows us to work with type information at runtime
data TermT (t :: T) where
  SInt :: TermT 'TInt
  SDouble :: TermT 'TDouble
  SBool :: TermT 'TBool
  SString :: TermT 'TString
  STuple :: TermT 'TTuple
  SUnit :: TermT 'TUnit
  SFun :: TermT a -> TermT b -> TermT ('TFun a b)  -- Function type constructor for single parameter

deriving instance Show (TermT t)

instance TestEquality TermT where
  testEquality :: TermT a -> TermT b -> Maybe (a :~: b)
  testEquality SInt SInt = Just Refl
  testEquality SDouble SDouble = Just Refl
  testEquality SBool SBool = Just Refl
  testEquality SString SString = Just Refl
  testEquality STuple STuple = Just Refl
  testEquality SUnit SUnit = Just Refl
  testEquality (SFun a b) (SFun c d) = case (testEquality a c, testEquality b d) of
    (Just Refl, Just Refl) -> Just Refl
    _ -> Nothing
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
  show (Exists (SFun a b)) = "Fun " <> show a <> " -> " <> show b

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
  TypeInferenceFailed :: Text -> TypeError
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
  RealType ('TFun a b) = RealType a -> RealType b

-- Type-safe operators
data Op (input :: T) (output :: T) where
  -- Arithmetic operators
  AddInt :: (input ~ 'TInt, output ~ 'TInt) => Op input output
  SubInt :: (input ~ 'TInt, output ~ 'TInt) => Op input output
  MulInt :: (input ~ 'TInt, output ~ 'TInt) => Op input output
  DivInt :: (input ~ 'TInt, output ~ 'TInt) => Op input output
  
  AddDouble :: (input ~ 'TDouble, output ~ 'TDouble) => Op input output
  SubDouble :: (input ~ 'TDouble, output ~ 'TDouble) => Op input output
  MulDouble :: (input ~ 'TDouble, output ~ 'TDouble) => Op input output
  DivDouble :: (input ~ 'TDouble, output ~ 'TDouble) => Op input output
  
  -- Unary operators
  PosInt :: (input ~ 'TInt, output ~ 'TInt) => Op input output
  NegInt :: (input ~ 'TInt, output ~ 'TInt) => Op input output
  
  PosDouble :: (input ~ 'TDouble, output ~ 'TDouble) => Op input output
  NegDouble :: (input ~ 'TDouble, output ~ 'TDouble) => Op input output
  
  -- Logical operators
  AndBool :: (input ~ 'TBool, output ~ 'TBool) => Op input output
  OrBool :: (input ~ 'TBool, output ~ 'TBool) => Op input output
  NotBool :: (input ~ 'TBool, output ~ 'TBool) => Op input output
  
  -- Equality operators
  EqInt :: (input ~ 'TInt, output ~ 'TBool) => Op input output
  NeInt :: (input ~ 'TInt, output ~ 'TBool) => Op input output
  
  EqDouble :: (input ~ 'TDouble, output ~ 'TBool) => Op input output
  NeDouble :: (input ~ 'TDouble, output ~ 'TBool) => Op input output
  
  EqBool :: (input ~ 'TBool, output ~ 'TBool) => Op input output
  NeBool :: (input ~ 'TBool, output ~ 'TBool) => Op input output
  
  EqString :: (input ~ 'TString, output ~ 'TBool) => Op input output
  NeString :: (input ~ 'TString, output ~ 'TBool) => Op input output

deriving instance Show (Op input output)

-- Typed expressions
data Expr t where
  ExprLit :: Show (RealType t) => TermT t -> RealType t -> Expr t
  ExprIdent :: Text -> Expr t
  ExprTuple :: [Exists Expr] -> Expr 'TTuple
  ExprUnary :: Op input output -> Expr input -> Expr output
  ExprBinary :: Op input output -> Expr input -> Expr input -> Expr output
  ExprApp :: Expr ('TFun a b) -> Expr a -> Expr b
  ExprIf :: Expr 'TBool -> Expr t -> Expr t -> Expr t
  ExprWhile :: Expr 'TBool -> Expr 'TUnit -> Expr 'TUnit
  ExprBlock :: [Exists Expr] -> Expr 'TUnit
  ExprUnit :: Expr 'TUnit
  ExprLet :: [(Text, Exists Expr)] -> Expr t -> Expr t
  ExprLambda :: Text -> TermT a -> Expr b -> Expr ('TFun a b)

instance Show (Expr t) where
  show (ExprLit ty val) = "ExprLit (" <> show ty <> ") " <> show val
  show (ExprIdent i) = "ExprIdent " <> unpack i
  show (ExprTuple es) = "ExprTuple " <> show es
  show (ExprUnary op e) = "ExprUnary " <> show op <> " (" <> show e <> ")"
  show (ExprBinary op e1 e2) = "ExprBinary " <> show op <> " (" <> show e1 <> ") (" <> show e2 <> ")"
  show (ExprApp f arg) = "ExprApp (" <> show f <> ") (" <> show arg <> ")"
  show (ExprIf cond thenExpr elseExpr) = "ExprIf (" <> show cond <> ") (" <> show thenExpr <> ") (" <> show elseExpr <> ")"
  show (ExprWhile cond body) = "ExprWhile (" <> show cond <> ") (" <> show body <> ")"
  show (ExprBlock es) = "ExprBlock " <> show es
  show ExprUnit = "ExprUnit"
  show (ExprLet bindings body) = "ExprLet " <> show bindings <> " (" <> show body <> ")"
  show (ExprLambda param ty body) = "ExprLambda " <> unpack param <> " : " <> show ty <> " -> " <> show body

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
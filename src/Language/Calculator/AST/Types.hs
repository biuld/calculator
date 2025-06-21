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
    T(..),
    getExprRange
) where

import Data.Text
import qualified Data.Map as Map
import Data.Type.Equality
import Data.Kind (Type)
import Language.Calculator.Common.Types (SourceToken(..), SourceRange(..))

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
  UnboundVariable :: SourceRange -> Text -> TypeError
  TypeMismatch :: SourceRange -> Exists TermT -> Exists TermT -> TypeError
  OpMismatch :: SourceRange -> Text -> Exists TermT -> TypeError
  ArgNumMismatch :: SourceRange -> Int -> Int -> TypeError
  NotAFunction :: SourceRange -> Text -> TypeError
  TypeInferenceFailed :: SourceRange -> Text -> TypeError
  OtherError :: SourceRange -> String -> TypeError
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
  ExprLit :: Show (RealType t) => SourceToken () -> TermT t -> RealType t -> Expr t
  ExprIdent :: SourceToken () -> Text -> Expr t
  ExprTuple :: SourceToken () -> [Exists Expr] -> Expr 'TTuple
  ExprUnary :: SourceToken () -> Op input output -> Expr input -> Expr output
  ExprBinary :: SourceToken () -> Op input output -> Expr input -> Expr input -> Expr output
  ExprApp :: SourceToken () -> Expr ('TFun a b) -> Expr a -> Expr b
  ExprIf :: SourceToken () -> Expr 'TBool -> Expr t -> Expr t -> Expr t
  ExprWhile :: SourceToken () -> Expr 'TBool -> Expr 'TUnit -> Expr 'TUnit
  ExprBlock :: SourceToken () -> [Exists Expr] -> Expr 'TUnit
  ExprUnit :: Expr 'TUnit
  ExprLet :: SourceToken () -> [(Text, Exists Expr)] -> Expr t -> Expr t
  ExprLambda :: SourceToken () -> Text -> TermT a -> Expr b -> Expr ('TFun a b)

instance Show (Expr t) where
  show (ExprLit _ ty val) = "ExprLit (" <> show ty <> ") " <> show val
  show (ExprIdent _ i) = "ExprIdent " <> unpack i
  show (ExprTuple _ es) = "ExprTuple " <> show es
  show (ExprUnary _ op e) = "ExprUnary " <> show op <> " (" <> show e <> ")"
  show (ExprBinary _ op e1 e2) = "ExprBinary " <> show op <> " (" <> show e1 <> ") (" <> show e2 <> ")"
  show (ExprApp _ f arg) = "ExprApp (" <> show f <> ") (" <> show arg <> ")"
  show (ExprIf _ cond thenExpr elseExpr) = "ExprIf (" <> show cond <> ") (" <> show thenExpr <> ") (" <> show elseExpr <> ")"
  show (ExprWhile _ cond body) = "ExprWhile (" <> show cond <> ") (" <> show body <> ")"
  show (ExprBlock _ es) = "ExprBlock " <> show es
  show ExprUnit = "ExprUnit"
  show (ExprLet _ bindings body) = "ExprLet " <> show bindings <> " (" <> show body <> ")"
  show (ExprLambda _ param ty body) = "ExprLambda " <> unpack param <> " : " <> show ty <> " -> " <> show body

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

getExprRange :: Expr t -> SourceRange
getExprRange (ExprLit (SourceToken r _) _ _) = r
getExprRange (ExprIdent (SourceToken r _) _) = r
getExprRange (ExprTuple (SourceToken r _) _) = r
getExprRange (ExprUnary (SourceToken r _) _ _) = r
getExprRange (ExprBinary (SourceToken r _) _ _ _) = r
getExprRange (ExprApp (SourceToken r _) _ _) = r
getExprRange (ExprIf (SourceToken r _) _ _ _) = r
getExprRange (ExprWhile (SourceToken r _) _ _) = r
getExprRange (ExprBlock (SourceToken r _) _) = r
getExprRange ExprUnit = error "ExprUnit does not have a source range."
getExprRange (ExprLet (SourceToken r _) _ _) = r
getExprRange (ExprLambda (SourceToken r _) _ _ _) = r 
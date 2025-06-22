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
    getExprRange,
    ExprBase(..),
    LitExpr(..),
    IdentExpr(..),
    TupleExpr(..),
    UnaryExpr(..),
    BinaryExpr(..),
    AppExpr(..),
    IfExpr(..),
    WhileExpr(..),
    BlockExpr(..),
    UnitExpr(..),
    LetExpr(..),
    LambdaExpr(..),
    SourceRange(..),
    getExprType
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

data ExprBase (t :: T) = ExprBase
  { exprSource :: SourceToken ()
  , exprType   :: TermT t
  }

-- Typed expressions
data Expr t where
  ExprLit :: Show (RealType t) => LitExpr t -> Expr t
  ExprIdent :: IdentExpr t -> Expr t
  ExprTuple :: TupleExpr 'TTuple -> Expr 'TTuple
  ExprUnary :: UnaryExpr input output -> Expr output
  ExprBinary :: BinaryExpr input output -> Expr output
  ExprApp :: AppExpr a b -> Expr b
  ExprIf :: IfExpr t -> Expr t
  ExprWhile :: WhileExpr 'TUnit -> Expr 'TUnit
  ExprBlock :: BlockExpr 'TUnit -> Expr 'TUnit
  ExprUnit :: UnitExpr 'TUnit -> Expr 'TUnit
  ExprLet :: LetExpr t -> Expr t
  ExprLambda :: LambdaExpr a b -> Expr ('TFun a b)

-- Individual expression records
data LitExpr (t :: T) = LitExpr
  { litBase  :: ExprBase t
  , litValue :: RealType t
  }

data IdentExpr (t :: T) = IdentExpr
  { identBase :: ExprBase t
  , identName :: Text
  }

data TupleExpr (t :: T) = TupleExpr
  { tupleBase  :: ExprBase t
  , tupleElems :: [Exists Expr]
  }

data UnaryExpr (input :: T) (output :: T) = UnaryExpr
  { unaryBase :: ExprBase output
  , unaryOp   :: Op input output
  , unaryExpr :: Expr input
  }

data BinaryExpr (input :: T) (output :: T) = BinaryExpr
  { binaryBase :: ExprBase output
  , binaryOp   :: Op input output
  , binaryLeft :: Expr input
  , binaryRight :: Expr input
  }

data AppExpr (a :: T) (b :: T) = AppExpr
  { appBase :: ExprBase b
  , appFun  :: Expr ('TFun a b)
  , appArg  :: Expr a
  }

data IfExpr (t :: T) = IfExpr
  { ifBase    :: ExprBase t
  , ifCond    :: Expr 'TBool
  , ifThen    :: Expr t
  , ifElse    :: Expr t
  }

data WhileExpr (t :: T) = WhileExpr
  { whileBase :: ExprBase t
  , whileCond :: Expr 'TBool
  , whileBody :: Expr t
  }

data BlockExpr (t :: T) = BlockExpr
  { blockBase :: ExprBase t
  , blockElems :: [Exists Expr]
  }

data UnitExpr (t :: T) = UnitExpr
  { unitBase :: ExprBase t
  }

data LetExpr (t :: T) = LetExpr
  { letBase    :: ExprBase t
  , letBindings :: [(Text, Exists Expr)]
  , letBody    :: Expr t
  }

data LambdaExpr (a :: T) (b :: T) = LambdaExpr
  { lambdaBase  :: ExprBase ('TFun a b)
  , lambdaParam :: Text
  , lambdaParamType :: TermT a
  , lambdaBody  :: Expr b
  }

-- | Get the type of an expression
getExprType :: Exists Expr -> Exists TermT
getExprType (Exists (ExprLit l)) = Exists (litBase l).exprType
getExprType (Exists (ExprIdent i)) = Exists (identBase i).exprType
getExprType (Exists (ExprTuple t)) = Exists (tupleBase t).exprType
getExprType (Exists (ExprUnary u)) = Exists (unaryBase u).exprType
getExprType (Exists (ExprBinary b)) = Exists (binaryBase b).exprType
getExprType (Exists (ExprApp a)) = Exists (appBase a).exprType
getExprType (Exists (ExprIf i)) = Exists (ifBase i).exprType
getExprType (Exists (ExprWhile w)) = Exists (whileBase w).exprType
getExprType (Exists (ExprBlock b)) = Exists (blockBase b).exprType
getExprType (Exists (ExprUnit u)) = Exists (unitBase u).exprType
getExprType (Exists (ExprLet l)) = Exists (letBase l).exprType
getExprType (Exists (ExprLambda l)) = Exists (lambdaBase l).exprType

instance Show (Expr t) where
  show (ExprLit (LitExpr base val)) = "ExprLit (" <> show base.exprType <> ") " <> show val
  show (ExprIdent (IdentExpr base i)) = "ExprIdent (" <> show base.exprType <> ") " <> unpack i
  show (ExprTuple (TupleExpr base es)) = "ExprTuple (" <> show base.exprType <> ") " <> show es
  show (ExprUnary (UnaryExpr base op e)) = "ExprUnary (" <> show base.exprType <> ") " <> show op <> " (" <> show e <> ")"
  show (ExprBinary (BinaryExpr base op e1 e2)) = "ExprBinary (" <> show base.exprType <> ") " <> show op <> " (" <> show e1 <> ") (" <> show e2 <> ")"
  show (ExprApp (AppExpr base f arg)) = "ExprApp (" <> show base.exprType <> ") (" <> show f <> ") (" <> show arg <> ")"
  show (ExprIf (IfExpr base cond thenExpr elseExpr)) = "ExprIf (" <> show base.exprType <> ") " <> show cond <> " (" <> show thenExpr <> ") (" <> show elseExpr <> ")"
  show (ExprWhile (WhileExpr base cond body)) = "ExprWhile (" <> show base.exprType <> ") " <> show cond <> " (" <> show body <> ")"
  show (ExprBlock (BlockExpr base es)) = "ExprBlock (" <> show base.exprType <> ") " <> show es
  show (ExprUnit (UnitExpr base)) = "ExprUnit (" <> show base.exprType <> ")"
  show (ExprLet (LetExpr base bindings body)) = "ExprLet (" <> show base.exprType <> ") " <> show bindings <> " (" <> show body <> ")"
  show (ExprLambda (LambdaExpr base param paramTy body)) = "ExprLambda (" <> show base.exprType <> ") " <> unpack param <> " : " <> show paramTy <> " -> " <> show body

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
getExprRange (ExprLit (LitExpr base _)) = base.exprSource.tokRange
getExprRange (ExprIdent (IdentExpr base _)) = base.exprSource.tokRange
getExprRange (ExprTuple (TupleExpr base _)) = base.exprSource.tokRange
getExprRange (ExprUnary (UnaryExpr base _ _)) = base.exprSource.tokRange
getExprRange (ExprBinary (BinaryExpr base _ _ _)) = base.exprSource.tokRange
getExprRange (ExprApp (AppExpr base _ _)) = base.exprSource.tokRange
getExprRange (ExprIf (IfExpr base _ _ _)) = base.exprSource.tokRange
getExprRange (ExprWhile (WhileExpr base _ _)) = base.exprSource.tokRange
getExprRange (ExprBlock (BlockExpr base _)) = base.exprSource.tokRange
getExprRange (ExprUnit (UnitExpr base)) = base.exprSource.tokRange
getExprRange (ExprLet (LetExpr base _ _)) = base.exprSource.tokRange
getExprRange (ExprLambda (LambdaExpr base _ _ _)) = base.exprSource.tokRange 
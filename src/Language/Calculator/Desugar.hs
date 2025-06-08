module Language.Calculator.Desugar (desugar, TypedExpr(..), module Language.Calculator.AST.Types) where

import qualified Language.Calculator.CST.Types as CST
import Language.Calculator.AST.Types
import qualified Data.Map as Map
import Data.Type.Equality

data TypedExpr where
  TypedExpr :: (Show (Sing t)) => STy t -> Expr t -> TypedExpr

-- The main desugaring function that converts CST to a type-checked AST.
desugar :: TypeEnv -> CST.Expr -> Either TypeError TypedExpr
desugar env cstExpr = case cstExpr of
  CST.ExprInt t -> Right $ TypedExpr SInt (ExprInt t.tokValue)
  CST.ExprDouble t -> Right $ TypedExpr SDouble (ExprDouble t.tokValue)
  CST.ExprBool t -> Right $ TypedExpr SBool (ExprBool t.tokValue)
  CST.ExprString t -> Right $ TypedExpr SString (ExprString t.tokValue)

  CST.ExprIdent t -> let name = t.tokValue in
    case Map.lookup name env of
      Nothing -> Left $ UnboundVariable name
      Just TInt -> Right $ TypedExpr SInt (ExprIdent name)
      Just TDouble -> Right $ TypedExpr SDouble (ExprIdent name)
      Just TBool -> Right $ TypedExpr SBool (ExprIdent name)
      Just TString -> Right $ TypedExpr SString (ExprIdent name)
      Just TTuple -> Right $ TypedExpr STuple (ExprIdent name)
      Just TUnit -> Right $ TypedExpr SUnit (ExprIdent name)

  CST.ExprUnary opToken cstE -> do
    TypedExpr sty e <- desugar env cstE
    let op = opToken.tokValue
    case (op, sty) of
      (CST.OpNot, SBool) -> Right $ TypedExpr SBool (ExprUnary OpNot e)
      (CST.OpMinus, SDouble) -> Right $ TypedExpr SDouble (ExprUnary OpMinus e)
      (CST.OpMinus, SInt) -> Right $ TypedExpr SInt (ExprUnary OpMinusInt e)
      _ -> Left $ OpMismatch (CST.opToText op) (fromSTy sty)

  CST.ExprBinary opToken cstE1 cstE2 -> do
    TypedExpr sty1 e1 <- desugar env cstE1
    TypedExpr sty2 e2 <- desugar env cstE2
    let op = opToken.tokValue
    case testEquality sty1 sty2 of
      Nothing -> Left $ TypeMismatch (fromSTy sty1) (fromSTy sty2)
      Just Refl -> case (op, sty1) of
        (CST.OpPlus, SInt) -> Right $ TypedExpr SInt (ExprBinary OpPlusInt e1 e2)
        (CST.OpPlus, SDouble) -> Right $ TypedExpr SDouble (ExprBinary OpPlus e1 e2)
        (CST.OpMinus, SInt) -> Right $ TypedExpr SInt (ExprBinary OpMinusInt e1 e2)
        (CST.OpMinus, SDouble) -> Right $ TypedExpr SDouble (ExprBinary OpMinus e1 e2)
        (CST.OpMultiply, SInt) -> Right $ TypedExpr SInt (ExprBinary OpMultiplyInt e1 e2)
        (CST.OpMultiply, SDouble) -> Right $ TypedExpr SDouble (ExprBinary OpMultiply e1 e2)
        (CST.OpDivide, SInt) -> Right $ TypedExpr SInt (ExprBinary OpDivideInt e1 e2)
        (CST.OpDivide, SDouble) -> Right $ TypedExpr SDouble (ExprBinary OpDivide e1 e2)
        (CST.OpEqual, SInt) -> Right $ TypedExpr SBool (ExprBinary OpEqualInt e1 e2)
        (CST.OpEqual, SDouble) -> Right $ TypedExpr SBool (ExprBinary OpEqualDouble e1 e2)
        (CST.OpEqual, SBool) -> Right $ TypedExpr SBool (ExprBinary OpEqualBool e1 e2)
        (CST.OpEqual, SString) -> Right $ TypedExpr SBool (ExprBinary OpEqualString e1 e2)
        (CST.OpNotEqual, SInt) -> Right $ TypedExpr SBool (ExprBinary OpNotEqualInt e1 e2)
        (CST.OpNotEqual, SDouble) -> Right $ TypedExpr SBool (ExprBinary OpNotEqualDouble e1 e2)
        (CST.OpNotEqual, SBool) -> Right $ TypedExpr SBool (ExprBinary OpNotEqualBool e1 e2)
        (CST.OpNotEqual, SString) -> Right $ TypedExpr SBool (ExprBinary OpNotEqualString e1 e2)
        (CST.OpAnd, SBool) -> Right $ TypedExpr SBool (ExprBinary OpAnd e1 e2)
        (CST.OpOr, SBool) -> Right $ TypedExpr SBool (ExprBinary OpOr e1 e2)
        _ -> Left $ OpMismatch (CST.opToText op) (fromSTy sty1)

  _ -> Left $ OtherError "Not implemented yet"
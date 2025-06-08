module Language.Calculator.Desugar (desugar) where

import qualified Language.Calculator.CST.Types as CST
import Language.Calculator.AST.Types
import qualified Data.Map as Map
import Data.Type.Equality

-- The main desugaring function that converts CST to a type-checked AST.
desugar :: TypeEnv -> CST.Expr -> Either TypeError TypeExpr
desugar env cstExpr = case cstExpr of
  CST.ExprInt t -> Right $ TypeExpr SInt (ExprInt t.tokValue)
  CST.ExprDouble t -> Right $ TypeExpr SDouble (ExprDouble t.tokValue)
  CST.ExprBool t -> Right $ TypeExpr SBool (ExprBool t.tokValue)
  CST.ExprString t -> Right $ TypeExpr SString (ExprString t.tokValue)

  CST.ExprIdent t -> let name = t.tokValue in
    case Map.lookup name env of
      Nothing -> Left $ UnboundVariable name
      Just (TypeExpr SInt _) -> Right $ TypeExpr SInt (ExprIdent name)
      Just (TypeExpr SDouble _) -> Right $ TypeExpr SDouble (ExprIdent name)
      Just (TypeExpr SBool _) -> Right $ TypeExpr SBool (ExprIdent name)
      Just (TypeExpr SString _) -> Right $ TypeExpr SString (ExprIdent name)
      Just (TypeExpr STuple _) -> Right $ TypeExpr STuple (ExprIdent name)
      Just (TypeExpr SUnit _) -> Right $ TypeExpr SUnit (ExprIdent name)

  CST.ExprUnary opToken cstE -> do
    TypeExpr sty e <- desugar env cstE
    let op = opToken.tokValue
    case (op, sty) of
      (CST.OpNot, SBool) -> Right $ TypeExpr SBool (ExprUnary OpNot e)
      (CST.OpPlus, SDouble) -> Right $ TypeExpr SDouble (ExprUnary OpDoublePos e)
      (CST.OpMinus, SDouble) -> Right $ TypeExpr SDouble (ExprUnary OpDoubleNeg e)
      (CST.OpPlus, SInt) -> Right $ TypeExpr SInt (ExprUnary OpIntPos e)
      (CST.OpMinus, SInt) -> Right $ TypeExpr SInt (ExprUnary OpIntNeg e)
      _ -> Left $ OpMismatch (CST.opToText op) (Exists sty)

  CST.ExprBinary opToken cstE1 cstE2 -> do
    TypeExpr sty1 e1 <- desugar env cstE1
    TypeExpr sty2 e2 <- desugar env cstE2
    let op = opToken.tokValue
    case testEquality sty1 sty2 of
      Nothing -> Left $ TypeMismatch (Exists sty1) (Exists sty2)
      Just Refl -> case (op, sty1) of
        (CST.OpPlus, SInt) -> Right $ TypeExpr SInt (ExprBinary OpIntAdd e1 e2)
        (CST.OpPlus, SDouble) -> Right $ TypeExpr SDouble (ExprBinary OpDoubleAdd e1 e2)
        (CST.OpMinus, SInt) -> Right $ TypeExpr SInt (ExprBinary OpIntSub e1 e2)
        (CST.OpMinus, SDouble) -> Right $ TypeExpr SDouble (ExprBinary OpDoubleSub e1 e2)
        (CST.OpMultiply, SInt) -> Right $ TypeExpr SInt (ExprBinary OpIntMul e1 e2)
        (CST.OpMultiply, SDouble) -> Right $ TypeExpr SDouble (ExprBinary OpDoubleMul e1 e2)
        (CST.OpDivide, SInt) -> Right $ TypeExpr SInt (ExprBinary OpIntDiv e1 e2)
        (CST.OpDivide, SDouble) -> Right $ TypeExpr SDouble (ExprBinary OpDoubleDiv e1 e2)
        (CST.OpEqual, SInt) -> Right $ TypeExpr SBool (ExprBinary OpIntEq e1 e2)
        (CST.OpEqual, SDouble) -> Right $ TypeExpr SBool (ExprBinary OpDoubleEq e1 e2)
        (CST.OpEqual, SBool) -> Right $ TypeExpr SBool (ExprBinary OpBoolEq e1 e2)
        (CST.OpEqual, SString) -> Right $ TypeExpr SBool (ExprBinary OpStringEq e1 e2)
        (CST.OpNotEqual, SInt) -> Right $ TypeExpr SBool (ExprBinary OpIntNe e1 e2)
        (CST.OpNotEqual, SDouble) -> Right $ TypeExpr SBool (ExprBinary OpDoubleNe e1 e2)
        (CST.OpNotEqual, SBool) -> Right $ TypeExpr SBool (ExprBinary OpBoolNe e1 e2)
        (CST.OpNotEqual, SString) -> Right $ TypeExpr SBool (ExprBinary OpStringNe e1 e2)
        (CST.OpAnd, SBool) -> Right $ TypeExpr SBool (ExprBinary OpAnd e1 e2)
        (CST.OpOr, SBool) -> Right $ TypeExpr SBool (ExprBinary OpOr e1 e2)
        _ -> Left $ OpMismatch (CST.opToText op) (Exists sty1)

  -- If-else statement
  CST.ExprIf cond thenExpr elseExpr -> do
    TypeExpr condTy cond' <- desugar env cond
    case condTy of
      SBool -> do
        TypeExpr thenTy then' <- desugar env thenExpr
        TypeExpr elseTy else' <- desugar env elseExpr
        case testEquality thenTy elseTy of
          Nothing -> Left $ TypeMismatch (Exists thenTy) (Exists elseTy)
          Just Refl -> Right $ TypeExpr thenTy (ExprIf cond' then' else')
      _ -> Left $ OpMismatch "if condition" (Exists condTy)

  -- While loop
  CST.ExprWhile cond body -> do
    TypeExpr condTy cond' <- desugar env cond
    case condTy of
      SBool -> do
        TypeExpr bodyTy body' <- desugar env body
        case bodyTy of
          SUnit -> Right $ TypeExpr SUnit (ExprWhile cond' body')
          _ -> Left $ OpMismatch "while body" (Exists bodyTy)
      _ -> Left $ OpMismatch "while condition" (Exists condTy)

  -- Block of expressions
  CST.ExprBlock exprs -> do
    desugaredExprs <- mapM (desugar env) exprs
    case desugaredExprs of
      [] -> Right $ TypeExpr SUnit ExprUnit
      [TypeExpr ty e] -> Right $ TypeExpr ty e
      _ -> Right $ TypeExpr SUnit (ExprBlock (map (\(TypeExpr _ e) -> Exists e) desugaredExprs))

  -- Tuple expression
  CST.ExprTuple exprs -> do
    desugaredExprs <- mapM (desugar env) exprs
    Right $ TypeExpr STuple (ExprTuple (map (\(TypeExpr _ e) -> Exists e) desugaredExprs))

  -- Function application
  CST.ExprApp fToken args -> do
    let fName = fToken.tokValue
    case Map.lookup fName env of
      Nothing -> Left $ UnboundVariable fName
      Just (TypeExpr _ _) -> do
        desugaredArgs <- mapM (desugar env) args
        Right $ TypeExpr SUnit (ExprApp fName (map (\(TypeExpr _ e) -> Exists e) desugaredArgs))

  -- Let binding
  CST.ExprLet nameToken valueExpr bodyExpr -> do
    let name = nameToken.tokValue
    TypeExpr valueTy valueAst <- desugar env valueExpr
    let newEnv = Map.insert name (TypeExpr valueTy valueAst) env
    TypeExpr bodyTy bodyAst <- desugar newEnv bodyExpr
    Right $ TypeExpr bodyTy (ExprLet name (Exists valueAst) bodyAst)
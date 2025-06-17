module Language.Calculator.Desugar (desugar) where

import qualified Language.Calculator.CST.Types as CST
import Language.Calculator.AST.Types
import qualified Data.Map as Map
import Data.Type.Equality
import qualified Data.Text as Text
import Control.Monad (foldM)

-- The main desugaring function that converts CST to a type-checked AST.
desugar :: TypeEnv -> CST.Expr -> Either TypeError TypeExpr
desugar env cstExpr = case cstExpr of
  CST.ExprInt t -> Right $ TypeExpr SInt (ExprLit SInt t.tokValue)
  CST.ExprDouble t -> Right $ TypeExpr SDouble (ExprLit SDouble t.tokValue)
  CST.ExprBool t -> Right $ TypeExpr SBool (ExprLit SBool t.tokValue)
  CST.ExprString t -> Right $ TypeExpr SString (ExprLit SString t.tokValue)

  CST.ExprIdent t -> let name = t.tokValue in
    case Map.lookup name env of
      Nothing -> Left $ UnboundVariable name
      Just (TypeExpr SInt _) -> Right $ TypeExpr SInt (ExprIdent name)
      Just (TypeExpr SDouble _) -> Right $ TypeExpr SDouble (ExprIdent name)
      Just (TypeExpr SBool _) -> Right $ TypeExpr SBool (ExprIdent name)
      Just (TypeExpr SString _) -> Right $ TypeExpr SString (ExprIdent name)
      Just (TypeExpr STuple _) -> Right $ TypeExpr STuple (ExprIdent name)
      Just (TypeExpr SUnit _) -> Right $ TypeExpr SUnit (ExprIdent name)
      Just (TypeExpr (SFun paramTy retTy) _) -> Right $ TypeExpr (SFun paramTy retTy) (ExprIdent name)

  CST.ExprUnary opToken cstE -> do
    TypeExpr sty e <- desugar env cstE
    let op = opToken.tokValue
    case (op, sty) of
      (CST.OpNot, SBool) -> Right $ TypeExpr SBool (ExprUnary NotBool e)
      (CST.OpPlus, SDouble) -> Right $ TypeExpr SDouble (ExprUnary PosDouble e)
      (CST.OpMinus, SDouble) -> Right $ TypeExpr SDouble (ExprUnary NegDouble e)
      (CST.OpPlus, SInt) -> Right $ TypeExpr SInt (ExprUnary PosInt e)
      (CST.OpMinus, SInt) -> Right $ TypeExpr SInt (ExprUnary NegInt e)
      _ -> Left $ OpMismatch (CST.opToText op) (Exists sty)

  CST.ExprBinary opToken cstE1 cstE2 -> do
    TypeExpr sty1 e1 <- desugar env cstE1
    TypeExpr sty2 e2 <- desugar env cstE2
    let op = opToken.tokValue
    case testEquality sty1 sty2 of
      Nothing -> Left $ TypeMismatch (Exists sty1) (Exists sty2)
      Just Refl -> case (op, sty1) of
        (CST.OpPlus, SInt) -> Right $ TypeExpr SInt (ExprBinary AddInt e1 e2)
        (CST.OpPlus, SDouble) -> Right $ TypeExpr SDouble (ExprBinary AddDouble e1 e2)
        (CST.OpMinus, SInt) -> Right $ TypeExpr SInt (ExprBinary SubInt e1 e2)
        (CST.OpMinus, SDouble) -> Right $ TypeExpr SDouble (ExprBinary SubDouble e1 e2)
        (CST.OpMultiply, SInt) -> Right $ TypeExpr SInt (ExprBinary MulInt e1 e2)
        (CST.OpMultiply, SDouble) -> Right $ TypeExpr SDouble (ExprBinary MulDouble e1 e2)
        (CST.OpDivide, SInt) -> Right $ TypeExpr SInt (ExprBinary DivInt e1 e2)
        (CST.OpDivide, SDouble) -> Right $ TypeExpr SDouble (ExprBinary DivDouble e1 e2)
        (CST.OpEqual, SInt) -> Right $ TypeExpr SBool (ExprBinary EqInt e1 e2)
        (CST.OpEqual, SDouble) -> Right $ TypeExpr SBool (ExprBinary EqDouble e1 e2)
        (CST.OpEqual, SBool) -> Right $ TypeExpr SBool (ExprBinary EqBool e1 e2)
        (CST.OpEqual, SString) -> Right $ TypeExpr SBool (ExprBinary EqString e1 e2)
        (CST.OpNotEqual, SInt) -> Right $ TypeExpr SBool (ExprBinary NeInt e1 e2)
        (CST.OpNotEqual, SDouble) -> Right $ TypeExpr SBool (ExprBinary NeDouble e1 e2)
        (CST.OpNotEqual, SBool) -> Right $ TypeExpr SBool (ExprBinary NeBool e1 e2)
        (CST.OpNotEqual, SString) -> Right $ TypeExpr SBool (ExprBinary NeString e1 e2)
        (CST.OpAnd, SBool) -> Right $ TypeExpr SBool (ExprBinary AndBool e1 e2)
        (CST.OpOr, SBool) -> Right $ TypeExpr SBool (ExprBinary OrBool e1 e2)
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
    -- In CST, the function part is an identifier token. We wrap it in a CST.ExprIdent
    -- to treat it uniformly as an expression, which aligns with our AST design where
    -- functions are first-class and any expression can be in the function position.
    let fExpr = CST.ExprIdent fToken
    -- First, desugar the function part
    desugaredFunc <- desugar env fExpr
    -- Fold over the arguments, applying one at a time
    let applyArg (TypeExpr fTy fAst) argCst = do
          case fTy of
            SFun paramTy retTy -> do
              TypeExpr argTy argAst <- desugar env argCst
              case testEquality argTy paramTy of
                Nothing -> Left $ TypeMismatch (Exists argTy) (Exists paramTy)
                Just Refl -> Right $ TypeExpr retTy (ExprApp fAst argAst)
            _ -> Left $ NotAFunction (Text.pack $ show fAst)
    -- Start with the desugared function, and fold the arguments onto it
    foldM applyArg desugaredFunc args

  -- Let binding
  CST.ExprLet bindings bodyExpr -> do
    -- First desugar all the bindings
    desugaredBindings <- mapM (\(nameToken, valueExpr) -> do
      let name = nameToken.tokValue
      TypeExpr valueTy valueAst <- desugar env valueExpr
      return (name, TypeExpr valueTy valueAst)) bindings

    -- Create new environment with all bindings
    let newEnv = foldl (\e (name, typeExpr) -> Map.insert name typeExpr e) env desugaredBindings

    -- Desugar the body with the new environment
    TypeExpr bodyTy bodyAst <- desugar newEnv bodyExpr

    -- Convert bindings to AST format
    let astBindings = map (\(name, TypeExpr _ ast) -> (name, Exists ast)) desugaredBindings

    Right $ TypeExpr bodyTy (ExprLet astBindings bodyAst)

  -- Lambda expression
  CST.ExprLambda params body -> do
    -- Helper to get type from CST or return error
    let getParamTy :: CST.Expr -> Either TypeError (Exists TermT)
        getParamTy (CST.ExprIdent t) = case t.tokValue of
          "Int" -> Right $ Exists SInt
          "Double" -> Right $ Exists SDouble
          "Bool" -> Right $ Exists SBool
          "String" -> Right $ Exists SString
          _ -> Left $ OtherError $ "Invalid type name '" ++ Text.unpack t.tokValue ++ "'. Expected Int, Double, Bool, or String."
        getParamTy _ = Left $ OtherError "Parameter type annotation must be an identifier."

    -- Process parameters, extracting their names and types
    paramList <- mapM (\(tok, tyExpr) -> do
      Exists pTy <- getParamTy tyExpr
      return (tok.tokValue, Exists pTy)) params
    
    -- Create the new environment for the lambda body
    let newEnv = foldl (\e (name, Exists ty) -> Map.insert name (TypeExpr ty (ExprIdent name)) e) env paramList

    -- Desugar the body with the new environment
    TypeExpr bodyTy bodyAst <- desugar newEnv body
    
    -- Build nested lambdas by folding from the right
    let buildLambda (name, Exists paramTy) (TypeExpr accTy accExpr) =
          TypeExpr (SFun paramTy accTy) (ExprLambda name paramTy accExpr)

    Right $ foldr buildLambda (TypeExpr bodyTy bodyAst) paramList
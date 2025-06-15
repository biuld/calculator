module Language.Calculator.Desugar (desugar) where

import qualified Language.Calculator.CST.Types as CST
import Language.Calculator.AST.Types
import qualified Data.Map as Map
import Data.Type.Equality
import qualified Data.Text as Text
import Data.Maybe (isJust, fromJust)
import Data.List (partition)

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
    let fName = fToken.tokValue
    case Map.lookup fName env of
      Nothing -> Left $ UnboundVariable fName
      Just (TypeExpr initialFTy _) -> do
        -- Recursive helper to apply arguments one by one
        let applyArgs :: TermT t1 -> [CST.Expr] -> Either TypeError (Exists TermT, [Exists Expr])
            applyArgs currentTy [] = Right (Exists currentTy, [])
            applyArgs (SFun paramTy retTy) (argCST:rest) = do
              TypeExpr argTy argAst <- desugar env argCST
              case testEquality argTy paramTy of
                Nothing -> Left $ TypeMismatch (Exists argTy) (Exists paramTy)
                Just Refl -> do
                  (Exists finalTy, restArgs) <- applyArgs retTy rest
                  return (Exists finalTy, Exists argAst : restArgs)
            applyArgs _ (_:_) = Left $ NotAFunction fName
        (Exists finalResultTy, desugaredArgsList) <- applyArgs initialFTy args
        return $ TypeExpr finalResultTy (ExprApp fName desugaredArgsList)

  -- Let binding
  CST.ExprLet nameToken valueExpr bodyExpr -> do
    let name = nameToken.tokValue
    TypeExpr valueTy valueAst <- desugar env valueExpr
    let newEnv = Map.insert name (TypeExpr valueTy valueAst) env
    TypeExpr bodyTy bodyAst <- desugar newEnv bodyExpr
    Right $ TypeExpr bodyTy (ExprLet [(name, Exists valueAst)] bodyAst)

  -- Lambda expression with multiple parameters
  CST.ExprLambda params body -> do
    -- This helper attempts to infer types for parameters without explicit annotations.
    -- It tries a list of possible types and returns the first successful combination.
    let inferAndDesugar :: [(Text.Text, [Exists TermT])]
                        -> TypeEnv
                        -> CST.Expr
                        -> Either TypeError ([(Text.Text, Exists TermT)], TypeExpr)
        inferAndDesugar [] env' expr' = do
          res <- desugar env' expr'
          return ([], res)
        inferAndDesugar ((name, tyTries):rest) env' expr' =
          let go [] = Left $ TypeInferenceFailed name
              go ((Exists ty):tys) =
                let newEnv = Map.insert name (TypeExpr ty (ExprIdent name)) env'
                in case inferAndDesugar rest newEnv expr' of
                  Right (inferredTys, res) -> Right ((name, Exists ty) : inferredTys, res)
                  Left _ -> go tys -- Backtrack on any failure
          in go tyTries

    let getParamTy :: CST.Expr -> Either TypeError (Maybe (Exists TermT))
        getParamTy (CST.ExprIdent t) = case t.tokValue of
          "Int" -> Right (Just (Exists SInt))
          "Double" -> Right (Just (Exists SDouble))
          "Bool" -> Right (Just (Exists SBool))
          "String" -> Right (Just (Exists SString))
          "Any" -> Right Nothing -- Represents a type to be inferred
          _ -> Left $ OtherError $ "Invalid type name '" ++ Text.unpack t.tokValue ++ "'. Expected one of: Int, Double, Bool, String"
        getParamTy _ = Left $ OtherError "Parameter type annotation must be an identifier"

    -- Extract and process parameters in one go
    paramListWithMaybeTypes <- mapM (\(tok, tyExpr) -> do
      pTyExists <- getParamTy tyExpr
      return (tok.tokValue, pTyExists)) params

    let (knownParams, unknownParams) = partition (isJust . snd) paramListWithMaybeTypes
        knownParamList = [ (n, ty) | (n, Just ty) <- knownParams ]
        unknownParamNames = map fst unknownParams
        envWithKnowns = foldl (\e (n, Exists ty) -> Map.insert n (TypeExpr ty (ExprIdent n)) e) env knownParamList
        possibleTypes = [Exists SInt, Exists SDouble, Exists SBool, Exists SString]
        inferenceTries = [(name, possibleTypes) | name <- unknownParamNames]

    -- Infer types and build final expression
    (inferredParams, TypeExpr bodyTy bodyExpr) <- inferAndDesugar inferenceTries envWithKnowns body

    -- Build nested lambdas and function type
    let buildLambda (name, Exists paramTy) (TypeExpr accTy accExpr) =
          TypeExpr (SFun paramTy accTy) (ExprLambda name (Exists paramTy) accExpr)
        inferredMap = Map.fromList inferredParams
        finalParamList = map (\(n, mTy) -> 
          maybe (n, fromJust (Map.lookup n inferredMap)) (\ty -> (n,ty)) mTy) 
          paramListWithMaybeTypes

    Right $ foldr buildLambda (TypeExpr bodyTy bodyExpr) finalParamList
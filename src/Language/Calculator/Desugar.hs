module Language.Calculator.Desugar (desugar) where

import qualified Language.Calculator.CST.Types as CST
import Language.Calculator.AST.Types
import qualified Data.Map as Map
import Data.Type.Equality
import Unsafe.Coerce (unsafeCoerce)

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
      Just (TypeExpr (SFun _ _) _) -> Right $ TypeExpr (SFun STuple SUnit) (ExprIdent name)

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
      Just (TypeExpr ty _) -> do
        -- Recursively check function type and arguments
        let checkArgs :: TermT t -> [CST.Expr] -> Either TypeError (TypeExpr, [Exists Expr])
            checkArgs (SFun paramTy retTy) (arg:rest) = do
              -- Desugar and type check the current argument
              TypeExpr argTy arg' <- desugar env arg
              -- Check argument type matches parameter type
              case testEquality argTy paramTy of
                Nothing -> Left $ TypeMismatch (Exists argTy) (Exists paramTy)
                Just Refl -> do
                  -- Recursively check remaining arguments
                  (TypeExpr finalTy finalExpr, restArgs) <- checkArgs retTy rest
                  return (TypeExpr finalTy finalExpr, Exists arg' : restArgs)
            checkArgs (SFun _ _) [] = Left $ ArgNumMismatch 1 0
            checkArgs finalTy [] = return (TypeExpr finalTy (ExprApp fName []), [])
            checkArgs _ _ = Left $ NotAFunction fName
        
        -- Helper function to count expected arguments
        let countArgs :: TermT t -> Int
            countArgs (SFun _ retTy) = 1 + countArgs retTy
            countArgs _ = 0
        
        -- Check if we have enough arguments
        let expectedArgs = countArgs ty
        if length args > expectedArgs then
          Left $ ArgNumMismatch expectedArgs (length args)
        else do
          -- Start checking from the function type
          (TypeExpr finalTy _, desugaredArgs) <- checkArgs ty args
          return $ TypeExpr finalTy (ExprApp fName desugaredArgs)

  -- Let binding
  CST.ExprLet nameToken valueExpr bodyExpr -> do
    let name = nameToken.tokValue
    TypeExpr valueTy valueAst <- desugar env valueExpr
    let newEnv = Map.insert name (TypeExpr valueTy valueAst) env
    TypeExpr bodyTy bodyAst <- desugar newEnv bodyExpr
    Right $ TypeExpr bodyTy (ExprLet [(name, Exists valueAst)] bodyAst)

  -- Lambda expression with multiple parameters
  CST.ExprLambda params body -> do
    -- Desugar the body first to get its type
    TypeExpr bodyTy body' <- desugar env body
    
    -- Convert multi-parameter lambda to nested single-parameter lambdas
    let desugarParams :: [(CST.SourceToken CST.Ident, CST.Expr)] -> Expr t -> Either TypeError TypeExpr
        desugarParams [] bodyExpr = Right $ TypeExpr bodyTy (unsafeCoerce bodyExpr)
        desugarParams ((param, tyExpr):rest) bodyExpr = do
            -- Desugar the parameter type
            TypeExpr paramTy _ <- desugar env tyExpr
            -- Create a single-parameter lambda
            let lambda = ExprLambda param.tokValue (Exists paramTy) bodyExpr
            -- Recursively process remaining parameters
            desugarParams rest lambda
    
    desugarParams params body'
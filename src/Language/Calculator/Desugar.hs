module Language.Calculator.Desugar (desugar) where

import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Type.Equality
import Language.Calculator.AST.Types as AST
import Language.Calculator.CST.Types qualified as CST
import Language.Calculator.Common.Types (SourceRange (..), SourceToken (..))

-- The main desugaring function that converts CST to a type-checked AST.
desugar :: TypeEnv -> CST.Expr -> Either AST.TypeError AST.TypeExpr
desugar env cstExpr = do
  let sourceToken = SourceToken (CST.getExprRange cstExpr) ()
  case cstExpr of
    CST.ExprInt t -> Right $ AST.TypeExpr AST.SInt (AST.ExprLit sourceToken AST.SInt t.tokValue)
    CST.ExprDouble t -> Right $ AST.TypeExpr AST.SDouble (AST.ExprLit sourceToken AST.SDouble t.tokValue)
    CST.ExprBool t -> Right $ AST.TypeExpr AST.SBool (AST.ExprLit sourceToken AST.SBool t.tokValue)
    CST.ExprString t -> Right $ AST.TypeExpr AST.SString (AST.ExprLit sourceToken AST.SString t.tokValue)
    CST.ExprIdent t ->
      let name = t.tokValue
       in case Map.lookup name env of
            Nothing -> Left $ AST.UnboundVariable t.tokRange name
            Just (AST.TypeExpr AST.SInt _) -> Right $ AST.TypeExpr AST.SInt (AST.ExprIdent sourceToken name)
            Just (AST.TypeExpr AST.SDouble _) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprIdent sourceToken name)
            Just (AST.TypeExpr AST.SBool _) -> Right $ AST.TypeExpr AST.SBool (AST.ExprIdent sourceToken name)
            Just (AST.TypeExpr AST.SString _) -> Right $ AST.TypeExpr AST.SString (AST.ExprIdent sourceToken name)
            Just (AST.TypeExpr AST.STuple _) -> Right $ AST.TypeExpr AST.STuple (AST.ExprIdent sourceToken name)
            Just (AST.TypeExpr AST.SUnit _) -> Right $ AST.TypeExpr AST.SUnit (AST.ExprIdent sourceToken name)
            Just (AST.TypeExpr (AST.SFun paramTy retTy) _) -> Right $ AST.TypeExpr (AST.SFun paramTy retTy) (AST.ExprIdent sourceToken name)
    CST.ExprUnary _ opToken cstE -> do
      AST.TypeExpr sty e <- desugar env cstE
      let op = opToken.tokValue
      case (op, sty) of
        (CST.OpNot, AST.SBool) -> Right $ AST.TypeExpr AST.SBool (AST.ExprUnary sourceToken AST.NotBool e)
        (CST.OpPlus, AST.SDouble) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprUnary sourceToken AST.PosDouble e)
        (CST.OpMinus, AST.SDouble) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprUnary sourceToken AST.NegDouble e)
        (CST.OpPlus, AST.SInt) -> Right $ AST.TypeExpr AST.SInt (AST.ExprUnary sourceToken AST.PosInt e)
        (CST.OpMinus, AST.SInt) -> Right $ AST.TypeExpr AST.SInt (AST.ExprUnary sourceToken AST.NegInt e)
        _ -> Left $ AST.OpMismatch opToken.tokRange (CST.opToText op) (AST.Exists sty)
    CST.ExprBinary _ opToken cstE1 cstE2 -> do
      AST.TypeExpr sty1 e1 <- desugar env cstE1
      AST.TypeExpr sty2 e2 <- desugar env cstE2
      let op = opToken.tokValue
      case testEquality sty1 sty2 of
        Nothing -> Left $ AST.TypeMismatch opToken.tokRange (AST.Exists sty1) (AST.Exists sty2)
        Just Refl -> case (op, sty1) of
          (CST.OpPlus, AST.SInt) -> Right $ AST.TypeExpr AST.SInt (AST.ExprBinary sourceToken AST.AddInt e1 e2)
          (CST.OpPlus, AST.SDouble) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprBinary sourceToken AST.AddDouble e1 e2)
          (CST.OpMinus, AST.SInt) -> Right $ AST.TypeExpr AST.SInt (AST.ExprBinary sourceToken AST.SubInt e1 e2)
          (CST.OpMinus, AST.SDouble) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprBinary sourceToken AST.SubDouble e1 e2)
          (CST.OpMultiply, AST.SInt) -> Right $ AST.TypeExpr AST.SInt (AST.ExprBinary sourceToken AST.MulInt e1 e2)
          (CST.OpMultiply, AST.SDouble) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprBinary sourceToken AST.MulDouble e1 e2)
          (CST.OpDivide, AST.SInt) -> Right $ AST.TypeExpr AST.SInt (AST.ExprBinary sourceToken AST.DivInt e1 e2)
          (CST.OpDivide, AST.SDouble) -> Right $ AST.TypeExpr AST.SDouble (AST.ExprBinary sourceToken AST.DivDouble e1 e2)
          (CST.OpEqual, AST.SInt) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.EqInt e1 e2)
          (CST.OpEqual, AST.SDouble) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.EqDouble e1 e2)
          (CST.OpEqual, AST.SBool) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.EqBool e1 e2)
          (CST.OpEqual, AST.SString) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.EqString e1 e2)
          (CST.OpNotEqual, AST.SInt) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.NeInt e1 e2)
          (CST.OpNotEqual, AST.SDouble) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.NeDouble e1 e2)
          (CST.OpNotEqual, AST.SBool) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.NeBool e1 e2)
          (CST.OpNotEqual, AST.SString) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.NeString e1 e2)
          (CST.OpAnd, AST.SBool) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.AndBool e1 e2)
          (CST.OpOr, AST.SBool) -> Right $ AST.TypeExpr AST.SBool (AST.ExprBinary sourceToken AST.OrBool e1 e2)
          _ -> Left $ AST.OpMismatch opToken.tokRange (CST.opToText op) (AST.Exists sty1)

    -- If-else statement
    CST.ExprIf _ cond thenExpr elseExpr -> do
      AST.TypeExpr condTy cond' <- desugar env cond
      case condTy of
        AST.SBool -> do
          AST.TypeExpr thenTy then' <- desugar env thenExpr
          AST.TypeExpr elseTy else' <- desugar env elseExpr
          case testEquality thenTy elseTy of
            Nothing -> Left $ AST.TypeMismatch (CST.getExprRange cstExpr) (AST.Exists thenTy) (AST.Exists elseTy)
            Just Refl -> Right $ AST.TypeExpr thenTy (AST.ExprIf sourceToken cond' then' else')
        _ -> Left $ AST.OpMismatch (CST.getExprRange cond) "if condition" (AST.Exists condTy)

    -- While loop
    CST.ExprWhile _ cond body -> do
      AST.TypeExpr condTy cond' <- desugar env cond
      case condTy of
        AST.SBool -> do
          AST.TypeExpr bodyTy body' <- desugar env body
          case bodyTy of
            AST.SUnit -> Right $ AST.TypeExpr AST.SUnit (AST.ExprWhile sourceToken cond' body')
            _ -> Left $ AST.OpMismatch (CST.getExprRange body) "while body" (AST.Exists bodyTy)
        _ -> Left $ AST.OpMismatch (CST.getExprRange cond) "while condition" (AST.Exists condTy)

    -- Block of expressions
    CST.ExprBlock _ exprs -> do
      desugaredExprs <- mapM (desugar env) exprs
      case desugaredExprs of
        [] -> Right $ AST.TypeExpr AST.SUnit AST.ExprUnit
        [AST.TypeExpr ty e] -> Right $ AST.TypeExpr ty e
        _ -> Right $ AST.TypeExpr AST.SUnit (AST.ExprBlock sourceToken (map (\(AST.TypeExpr _ e) -> AST.Exists e) desugaredExprs))

    -- Tuple expression
    CST.ExprTuple _ exprs -> do
      desugaredExprs <- mapM (desugar env) exprs
      Right $ AST.TypeExpr AST.STuple (AST.ExprTuple sourceToken (map (\(AST.TypeExpr _ e) -> AST.Exists e) desugaredExprs))

    -- Function application
    CST.ExprApp _ fToken args -> do
      -- In CST, the function part is an identifier token. We wrap it in a CST.ExprIdent
      -- to treat it uniformly as an expression, which aligns with our AST design where
      -- functions are first-class and any expression can be in the function position.
      let fExprCST = CST.ExprIdent fToken
      -- First, desugar the function part
      AST.TypeExpr fTy fAst <- desugar env fExprCST
      -- Fold over the arguments, applying one at a time
      let applyArg (AST.TypeExpr fTy' fAst') argCst = do
            case fTy' of
              AST.SFun paramTy retTy -> do
                AST.TypeExpr argTy argAst <- desugar env argCst
                case testEquality argTy paramTy of
                  Nothing -> Left $ AST.TypeMismatch (CST.getExprRange argCst) (AST.Exists argTy) (AST.Exists paramTy)
                  Just Refl ->
                    let appStart = (AST.getExprRange fAst').srcStart
                        appEnd = (AST.getExprRange argAst).srcEnd
                        appRange = SourceRange appStart appEnd
                        appSourceToken = SourceToken appRange ()
                     in Right $ AST.TypeExpr retTy (AST.ExprApp appSourceToken fAst' argAst)
              _ -> Left $ AST.NotAFunction (AST.getExprRange fAst') (Text.pack $ show fAst')
      -- Start with the desugared function, and fold the arguments onto it
      foldM applyArg (AST.TypeExpr fTy fAst) args

    -- Let binding
    CST.ExprLet _ bindings bodyExpr -> do
      -- First desugar all the bindings
      desugaredBindings <-
        mapM
          ( \(nameToken, valueExpr) -> do
              let name = nameToken.tokValue
              AST.TypeExpr valueTy valueAst <- desugar env valueExpr
              return (name, AST.TypeExpr valueTy valueAst)
          )
          bindings

      -- Create new environment with all bindings
      let newEnv = foldl (\e (name, typeExpr) -> Map.insert name typeExpr e) env desugaredBindings

      -- Desugar the body with the new environment
      AST.TypeExpr bodyTy bodyAst <- desugar newEnv bodyExpr

      -- Convert bindings to AST format
      let astBindings = map (\(name, AST.TypeExpr _ ast) -> (name, AST.Exists ast)) desugaredBindings

      Right $ AST.TypeExpr bodyTy (AST.ExprLet sourceToken astBindings bodyAst)

    -- Lambda expression
    CST.ExprLambda _ params body -> do
      -- Helper to get type from CST or return error
      let getParamTy :: CST.Expr -> Either AST.TypeError (AST.Exists AST.TermT)
          getParamTy (CST.ExprIdent t) =
            case t.tokValue of
              "Int" -> Right $ AST.Exists AST.SInt
              "Double" -> Right $ AST.Exists AST.SDouble
              "Bool" -> Right $ AST.Exists AST.SBool
              "String" -> Right $ AST.Exists AST.SString
              _ -> Left $ AST.OtherError t.tokRange $ "Invalid type name \'" ++ Text.unpack t.tokValue ++ "\'. Expected Int, Double, Bool, or String."
          getParamTy cstTyExpr = Left $ AST.OtherError (CST.getExprRange cstTyExpr) "Parameter type annotation must be an identifier."

      -- Process parameters, extracting their names and types
      paramList <-
        mapM
          ( \(tok, tyExpr) -> do
              AST.Exists pTy <- getParamTy tyExpr
              return (tok, AST.Exists pTy)
          )
          params -- Store the whole token, not just its value.

      -- Create the new environment for the lambda body
      let newEnv = foldl (\e (tok, AST.Exists ty) -> Map.insert tok.tokValue (AST.TypeExpr ty (AST.ExprIdent (SourceToken tok.tokRange ()) tok.tokValue)) e) env paramList

      -- Desugar the body with the new environment
      AST.TypeExpr bodyTy bodyAst <- desugar newEnv body

      -- Build nested lambdas by folding from the right
      let buildLambda (tok, AST.Exists paramTy) (AST.TypeExpr accTy accExpr) =
            let lambdaStart = tok.tokRange
                lambdaEnd = AST.getExprRange accExpr
                combinedRange = SourceRange lambdaStart.srcStart lambdaEnd.srcEnd
                lambdaSourceToken = SourceToken combinedRange ()
             in AST.TypeExpr (AST.SFun paramTy accTy) (AST.ExprLambda lambdaSourceToken tok.tokValue paramTy accExpr)

      Right $ foldr buildLambda (AST.TypeExpr bodyTy bodyAst) paramList
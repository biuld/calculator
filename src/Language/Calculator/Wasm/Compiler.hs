{-# LANGUAGE MultiWayIf #-}

module Language.Calculator.Wasm.Compiler
  ( compileToWasm,
  )
where

import Control.Monad.RWS.Strict
import Data.Map qualified as Map
import Data.Text (Text, pack)
import Data.Type.Equality (testEquality, (:~:) (Refl))
import Language.Calculator.AST.Types (AppExpr (..), BinaryExpr (..), Exists (..), Expr (..), IdentExpr (..), IfExpr (..), LambdaExpr (..), LetExpr (..), LitExpr (..), Op (..), TermT (..), exprType, getExprType)
import Language.Calculator.Wasm.CompileEnv (CompileEnv (..), addFunctionTypeSignature, extractFuncLocals, initialCompileEnv)
import Language.Calculator.Wasm.Types (WatExpr (..), WatFunction (..), WatLit (..), WatModule (..), WatOp (..), WatType (..), WatTypeSignature (..))

-- | Type alias for the Compiler Monad
type CompilerM = RWS () [WatFunction] CompileEnv

-- | Converts a Calculator TermT to a WAT type
termTToWatType :: Exists TermT -> WatType
termTToWatType (Exists SInt) = I32
termTToWatType (Exists SDouble) = F64
termTToWatType (Exists SBool) = I32 -- Booleans are typically represented as I32 (0 for false, 1 for true) in WASM
termTToWatType (Exists (SFun _ _)) = WatTypeRef "func"
termTToWatType (Exists SUnit) = Void -- Unit type, no value returned
termTToWatType _ = error "Unsupported Calculator TermT for WASM conversion"

-- | Helper function to compile a single let binding within an ExprLet
compileLetBinding :: (Text, Exists Expr) -> CompilerM ([WatExpr])
compileLetBinding (name, Exists expr) =
  do
    valExprs <- compileExpr (Exists expr)
    currentEnv <- get
    let bindingWatType = termTToWatType (getExprType (Exists expr))
    let updatedLocalMap = Map.insert name (Left bindingWatType) currentEnv.localMap
    let envAfterLocal = currentEnv {localMap = updatedLocalMap}
    case getExprType (Exists expr) of
      Exists (SFun _ _) -> do
        -- For functions, we update the local map with the function name directly.
        -- No WAT expressions are generated for the binding itself; the function is available by name.
        case valExprs of
            [WatRef fnName] -> put (envAfterLocal {localMap = Map.insert name (Right fnName) envAfterLocal.localMap})
            _ -> error "Expected a single WatRef for function binding"
        return [] -- Return empty list for function bindings
      _ -> do
        put envAfterLocal
        return (valExprs ++ [WatLocalSet name bindingWatType])

-- | Compiles a single expression to WAT expressions and accumulates functions
compileExpr :: Exists Expr -> CompilerM [WatExpr]
compileExpr (Exists (ExprLit (LitExpr base val))) =
  if
    | Just Refl <- testEquality base.exprType SInt -> return [WatConst (LitI32 val)]
    | Just Refl <- testEquality base.exprType SDouble -> return [WatConst (LitF64 val)]
    | Just Refl <- testEquality base.exprType SBool -> return [WatConst (LitI32 (if val then 1 else 0))]
    | otherwise -> error $ "Unsupported literal type for WASM compilation: " <> show base.exprType
compileExpr (Exists (ExprIdent (IdentExpr _ name))) =
  do
    env <- get
    case Map.lookup name env.localMap of
      Just (Left watTy) -> return [WatLocalGet name watTy]
      Just (Right funcName) -> return [WatRef funcName]
      Nothing -> error $ "Unbound identifier: " <> show name
compileExpr (Exists (ExprBinary (BinaryExpr _ op e1 e2))) =
  do
    watE1 <- compileExpr (Exists e1)
    watE2 <- compileExpr (Exists e2)
    let watOp = case op of
          AddInt -> I32Add
          SubInt -> I32Sub
          MulInt -> I32Mul
          DivInt -> I32DivS
          EqInt -> I32Eq
          NeInt -> I32Ne
          AddDouble -> F32Add
          SubDouble -> F32Sub
          MulDouble -> F32Mul
          DivDouble -> F32Div
          EqDouble -> F32Eq
          NeDouble -> F32Ne
          AndBool -> I32And
          OrBool -> I32Or
          _ -> error $ "Unsupported binary op for WAT: " <> show op
    return (watE1 ++ watE2 ++ [WatOp watOp []])
compileExpr (Exists (ExprLet (LetExpr _ bindings body))) =
  do
    initExprsList <- mapM compileLetBinding bindings
    let initExprs = concat initExprsList -- Flatten the list of lists of expressions
    bodyExprs <- compileExpr (Exists body)
    return (initExprs ++ bodyExprs)
compileExpr (Exists (ExprIf (IfExpr _ cond thenExpr elseExpr))) =
  do
    condWat <- compileExpr (Exists cond)
    let condExpr = case condWat of
          [c] -> c
          _ -> error "Conditional expression did not compile to a single WAT expression."
    thenWat <- compileExpr (Exists thenExpr)
    elseWat <- compileExpr (Exists elseExpr)
    return [WatIf condExpr thenWat (Just elseWat)]
compileExpr (Exists (ExprApp (AppExpr _ f arg))) =
  do
    argWat <- compileExpr (Exists arg)
    env <- get
    let funcName = case f of
          (ExprIdent (IdentExpr _ name)) -> case Map.lookup name env.localMap of
            Just (Right fn) -> fn
            _ -> error $ "Unsupported function expression in ExprApp: Expected function alias, got " <> show f
          _ -> error $ "Unsupported function expression in ExprApp: Expected ExprIdent, got " <> show f
    return [WatCall funcName argWat]
compileExpr (Exists (ExprLambda (LambdaExpr _ paramName paramTy body))) =
  do
    paramWatTy <- return (termTToWatType (Exists paramTy))
    funcName <- return ("lambda_" <> pack (show paramWatTy)) -- Generate a unique name

    -- Save current environment and set up new environment for function body
    originalEnv <- get
    -- Create a new environment for the function body by extending the original environment
    -- with the lambda's own parameter. This allows the body to access outer scope variables.
    let funcEnvForBody = originalEnv {localMap = Map.insert paramName (Left paramWatTy) originalEnv.localMap}
    put funcEnvForBody

    bodyWat <- compileExpr (Exists body)
    finalFuncEnv <- get -- Get the environment after compiling body

    -- Restore original environment
    put originalEnv

    let resultWatTy = termTToWatType (getExprType (Exists body))
    let funcSig = WatTypeSignature {sigParams = [paramWatTy], sigResults = [resultWatTy]}

    (typeAlias, envWithType) <- return (addFunctionTypeSignature originalEnv funcName funcSig) -- Use originalEnv
    put envWithType -- Update the main environment with the new type alias

    let lambdaFunc = WatFunction
          { funcName = funcName,
            funcTypeAlias = typeAlias,
            funcParams = [(paramName, paramWatTy)], -- Add the parameter here
            funcLocals = extractFuncLocals (Map.delete paramName finalFuncEnv.localMap), -- Ensure parameter is not a WAT local
            funcBody = bodyWat
          }
    tell [lambdaFunc] -- Add the new function to the Writer output
    return [WatRef funcName]
compileExpr (Exists e) = error $ "Unsupported expression for WAT compilation: " <> show e

-- | Compiles the main Calculator AST to a WAT module
compileToWasm :: Exists Expr -> WatModule
compileToWasm (Exists expr) =
  let (body, finalEnv, newFunctions) = runRWS (compileExpr (Exists expr)) () initialCompileEnv
      -- Determine the main function\'s result type
      mainResultType = termTToWatType (getExprType (Exists expr))

      -- Create the main function\'s type signature
      mainFuncSig = WatTypeSignature {sigParams = [], sigResults = [mainResultType]}
      (mainTypeAlias, envWithMainType) = addFunctionTypeSignature finalEnv "main" mainFuncSig

      mainFunc = 
        WatFunction
          { funcName = "main",
            funcTypeAlias = mainTypeAlias,
            funcParams = [], -- Main function has no parameters
            funcLocals = extractFuncLocals envWithMainType.localMap,
            funcBody = body
          }
      allFunctions = [mainFunc] ++ newFunctions
      -- Transform the type signatures map to have Text aliases as keys
      finalTypeSignatures = Map.foldrWithKey (\sig alias acc -> Map.insert alias sig acc) Map.empty envWithMainType.typeSignatures
   in WatModule {moduleTypes = finalTypeSignatures, moduleFunctions = allFunctions}
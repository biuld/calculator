{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Interpreter (
    interpret
) where

import Language.Calculator.AST.Types
import Language.Calculator.AST.Env (Env(..), lookupEnv, extendEnv)
import Data.Text (Text, unpack)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.Map as Map

-- Helper function to interpret Exists Expr
interpretSomeExpr :: Env -> Exists Expr -> Exists Expr
interpretSomeExpr env (Exists e) = Exists (interpret env e)

-- Helper function to apply arguments to a function
applyArgs :: forall t. Env -> Text -> Expr t -> [Exists Expr] -> Expr t
applyArgs env func (ExprLambda param _ body) args = case args of
    [] -> unsafeCoerce $ interpret env body  -- No arguments, directly interpret the body
    (Exists arg:rest) -> do
        -- Evaluate the current argument
        let evaluatedArg = interpret env arg
        -- Create a new environment with the parameter binding
        let newEnv = extendEnv [(param, TypeExpr (exprToTermT env evaluatedArg) evaluatedArg)] env
        -- If there are more arguments, recursively apply them
        case rest of
            [] -> unsafeCoerce $ interpret newEnv body
            _ -> case interpret newEnv body of
                nextFunc@(ExprLambda nextParam _ _) -> do
                    -- Create a nested environment for the next lambda
                    let nextEnv = Env Map.empty (Just newEnv)
                    -- Generate a unique identifier using the function name
                    let uniqueFuncName = func <> "_" <> nextParam
                    -- Store the next lambda in the nested environment using the unique identifier
                    let finalEnv = extendEnv [(uniqueFuncName, TypeExpr (exprToTermT newEnv nextFunc) nextFunc)] nextEnv
                    unsafeCoerce $ applyArgs finalEnv uniqueFuncName nextFunc rest
                _ -> error $ unpack func <> " expects more arguments"
applyArgs _ func _ _ = error $ unpack func <> " is not a function"

-- Main interpreter function
interpret :: forall t. Env -> Expr t -> Expr t
interpret env expr = case expr of
    -- Literals are already values
    ExprLit ty val -> ExprLit ty val
    
    -- Variable lookup
    ExprIdent name -> case lookupEnv name env of
        Just (TypeExpr _ e) -> 
            -- We need to ensure the type matches
            case testEquality (exprToTermT env e) (exprToTermT env expr) of
                Just Refl -> unsafeCoerce e
                Nothing -> error $ "Type mismatch in variable lookup: " ++ unpack name
        Nothing -> error $ "Unbound variable: " ++ unpack name
    
    -- Tuple evaluation
    ExprTuple exprs -> ExprTuple $ map (interpretSomeExpr env) exprs
    
    -- Unary operations
    ExprUnary op e -> case (op, interpret env e) of
        (NotBool, ExprLit SBool b) -> ExprLit SBool $ not b
        (PosDouble, ExprLit SDouble d) -> ExprLit SDouble d
        (NegDouble, ExprLit SDouble d) -> ExprLit SDouble $ -d
        (PosInt, ExprLit SInt i) -> ExprLit SInt i
        (NegInt, ExprLit SInt i) -> ExprLit SInt $ -i
        _ -> error "Invalid unary operation"
    
    -- Binary operations
    ExprBinary op e1 e2 -> case (op, interpret env e1, interpret env e2) of
        -- Arithmetic operations for Double
        (AddDouble, ExprLit SDouble d1, ExprLit SDouble d2) -> ExprLit SDouble $ d1 + d2
        (SubDouble, ExprLit SDouble d1, ExprLit SDouble d2) -> ExprLit SDouble $ d1 - d2
        (MulDouble, ExprLit SDouble d1, ExprLit SDouble d2) -> ExprLit SDouble $ d1 * d2
        (DivDouble, ExprLit SDouble d1, ExprLit SDouble d2) -> ExprLit SDouble $ d1 / d2
        
        -- Arithmetic operations for Int
        (AddInt, ExprLit SInt i1, ExprLit SInt i2) -> ExprLit SInt $ i1 + i2
        (SubInt, ExprLit SInt i1, ExprLit SInt i2) -> ExprLit SInt $ i1 - i2
        (MulInt, ExprLit SInt i1, ExprLit SInt i2) -> ExprLit SInt $ i1 * i2
        (DivInt, ExprLit SInt i1, ExprLit SInt i2) -> ExprLit SInt $ i1 `div` i2
        
        -- Logical operations
        (AndBool, ExprLit SBool b1, ExprLit SBool b2) -> ExprLit SBool $ b1 && b2
        (OrBool, ExprLit SBool b1, ExprLit SBool b2) -> ExprLit SBool $ b1 || b2
        
        -- Equality operations
        (EqInt, ExprLit SInt i1, ExprLit SInt i2) -> ExprLit SBool $ i1 == i2
        (NeInt, ExprLit SInt i1, ExprLit SInt i2) -> ExprLit SBool $ i1 /= i2
        (EqDouble, ExprLit SDouble d1, ExprLit SDouble d2) -> ExprLit SBool $ d1 == d2
        (NeDouble, ExprLit SDouble d1, ExprLit SDouble d2) -> ExprLit SBool $ d1 /= d2
        (EqBool, ExprLit SBool b1, ExprLit SBool b2) -> ExprLit SBool $ b1 == b2
        (NeBool, ExprLit SBool b1, ExprLit SBool b2) -> ExprLit SBool $ b1 /= b2
        (EqString, ExprLit SString s1, ExprLit SString s2) -> ExprLit SBool $ s1 == s2
        (NeString, ExprLit SString s1, ExprLit SString s2) -> ExprLit SBool $ s1 /= s2
        
        _ -> error "Invalid binary operation"
    
    -- Function application
    ExprApp func args -> case lookupEnv func env of
        Just (TypeExpr _ funcExpr) -> unsafeCoerce $ applyArgs env func funcExpr args
        _ -> error $ unpack func <> " is not a function"

    -- If-else statement
    ExprIf cond thenExpr elseExpr -> case interpret env cond of
        ExprLit SBool True -> interpret env thenExpr
        ExprLit SBool False -> interpret env elseExpr
        _ -> error "Condition must be a boolean"

    -- While loop
    ExprWhile cond body -> case interpret env cond of
        ExprLit SBool True -> do
            let _ = interpret env body
            interpret env expr  -- Recursively evaluate the while loop
        ExprLit SBool False -> ExprUnit
        _ -> error "While condition must be a boolean"

    -- Block of expressions
    ExprBlock exprs -> case exprs of
        [] -> ExprUnit
        [e] -> case interpretSomeExpr env e of
            Exists e' -> unsafeCoerce e'
        (e:es) -> do
            let _ = interpretSomeExpr env e
            interpret env (ExprBlock es)

    -- Let binding with multiple bindings
    ExprLet bindings body -> do
        let newBindings = map (\(name, value) -> 
                case interpretSomeExpr env value of
                    Exists e -> (name, TypeExpr (exprToTermT env e) e)
                ) bindings
        let newEnv = extendEnv newBindings env
        interpret newEnv body

    -- Lambda expression with parameters of different types
    ExprLambda param ty body -> ExprLambda param ty body  -- Lambda expressions are already values

    -- Unit value
    ExprUnit -> ExprUnit

-- Helper function to get the type of an expression
exprToTermT :: Env -> Expr t -> TermT t
exprToTermT _ (ExprLit ty _) = ty
exprToTermT _ (ExprTuple _) = STuple
exprToTermT env (ExprIdent name) = case lookupEnv name env of
    Just (TypeExpr t _) -> unsafeCoerce t
    Nothing -> error $ "Unbound variable: " ++ unpack name
exprToTermT _ (ExprUnary op _) = case op of
    NotBool -> SBool
    PosDouble -> SDouble
    NegDouble -> SDouble
    PosInt -> SInt
    NegInt -> SInt
    _ -> error "Unhandled unary operation"
exprToTermT _ (ExprBinary op _ _) = case op of
    AddInt -> SInt
    SubInt -> SInt
    MulInt -> SInt
    DivInt -> SInt
    AddDouble -> SDouble
    SubDouble -> SDouble
    MulDouble -> SDouble
    DivDouble -> SDouble
    AndBool -> SBool
    OrBool -> SBool
    EqInt -> SBool
    NeInt -> SBool
    EqDouble -> SBool
    NeDouble -> SBool
    EqBool -> SBool
    NeBool -> SBool
    EqString -> SBool
    NeString -> SBool
    _ -> error "Unhandled binary operation"
exprToTermT env (ExprApp func _) = case lookupEnv func env of
    Just (TypeExpr (SFun _ retTy) _) -> unsafeCoerce retTy
    _ -> error $ "Cannot determine type of function application: " ++ unpack func
exprToTermT env (ExprIf _ thenExpr _) = exprToTermT env thenExpr
exprToTermT _ (ExprWhile _ _) = SUnit
exprToTermT _ (ExprBlock _) = SUnit
exprToTermT _ ExprUnit = SUnit
exprToTermT env (ExprLet _ body) = exprToTermT env body
exprToTermT env (ExprLambda _ ty body) = case ty of
    Exists paramTy -> unsafeCoerce $ SFun paramTy (exprToTermT env body)
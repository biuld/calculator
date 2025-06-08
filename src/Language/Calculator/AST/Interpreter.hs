{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Interpreter (
    interpret
) where

import Language.Calculator.AST.Types
import Language.Calculator.AST.Env (Env, lookupEnv, extendEnv)
import Data.Text (unpack)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Unsafe.Coerce (unsafeCoerce)

-- Helper function to interpret Exists Expr
interpretSomeExpr :: Env -> Exists Expr -> Exists Expr
interpretSomeExpr env (Exists e) = Exists (interpret env e)

-- Main interpreter function
interpret :: forall t. Env -> Expr t -> Expr t
interpret env expr = case expr of
    -- Literals are already values
    ExprInt i -> ExprInt i
    ExprDouble d -> ExprDouble d
    ExprBool b -> ExprBool b
    ExprString s -> ExprString s
    
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
        (OpNot, ExprBool b) -> ExprBool $ not b
        (OpDoublePos, ExprDouble d) -> ExprDouble d
        (OpDoubleNeg, ExprDouble d) -> ExprDouble $ -d
        (OpIntPos, ExprInt i) -> ExprInt i
        (OpIntNeg, ExprInt i) -> ExprInt $ -i
        _ -> error "Invalid unary operation"
    
    -- Binary operations
    ExprBinary op e1 e2 -> case (op, interpret env e1, interpret env e2) of
        -- Arithmetic operations for Double
        (OpDoubleAdd, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 + d2
        (OpDoubleSub, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 - d2
        (OpDoubleMul, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 * d2
        (OpDoubleDiv, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 / d2
        
        -- Arithmetic operations for Int
        (OpIntAdd, ExprInt i1, ExprInt i2) -> ExprInt $ i1 + i2
        (OpIntSub, ExprInt i1, ExprInt i2) -> ExprInt $ i1 - i2
        (OpIntMul, ExprInt i1, ExprInt i2) -> ExprInt $ i1 * i2
        (OpIntDiv, ExprInt i1, ExprInt i2) -> ExprInt $ i1 `div` i2
        
        -- Logical operations
        (OpAnd, ExprBool b1, ExprBool b2) -> ExprBool $ b1 && b2
        (OpOr, ExprBool b1, ExprBool b2) -> ExprBool $ b1 || b2
        
        -- Equality operations
        (OpIntEq, ExprInt i1, ExprInt i2) -> ExprBool $ i1 == i2
        (OpIntNe, ExprInt i1, ExprInt i2) -> ExprBool $ i1 /= i2
        (OpDoubleEq, ExprDouble d1, ExprDouble d2) -> ExprBool $ d1 == d2
        (OpDoubleNe, ExprDouble d1, ExprDouble d2) -> ExprBool $ d1 /= d2
        (OpBoolEq, ExprBool b1, ExprBool b2) -> ExprBool $ b1 == b2
        (OpBoolNe, ExprBool b1, ExprBool b2) -> ExprBool $ b1 /= b2
        (OpStringEq, ExprString s1, ExprString s2) -> ExprBool $ s1 == s2
        (OpStringNe, ExprString s1, ExprString s2) -> ExprBool $ s1 /= s2
        
        _ -> error "Invalid binary operation"
    
    -- Function application (simplified for now)
    ExprApp f _ -> error $ "Function application not implemented: " ++ unpack f

    -- If-else statement
    ExprIf cond thenExpr elseExpr -> case interpret env cond of
        ExprBool True -> interpret env thenExpr
        ExprBool False -> interpret env elseExpr
        _ -> error "Condition must be a boolean"

    -- While loop
    ExprWhile cond body -> case interpret env cond of
        ExprBool True -> do
            let _ = interpret env body
            interpret env expr  -- Recursively evaluate the while loop
        ExprBool False -> ExprUnit
        _ -> error "While condition must be a boolean"

    -- Block of expressions
    ExprBlock exprs -> case exprs of
        [] -> ExprUnit
        [e] -> case interpretSomeExpr env e of
            Exists e' -> unsafeCoerce e'
        (e:es) -> do
            let _ = interpretSomeExpr env e
            interpret env (ExprBlock es)

    -- Let binding
    ExprLet name value body ->
        case interpretSomeExpr env value of
            Exists e ->
                let coerced = unsafeCoerce e :: Expr t
                    typeExpr = TypeExpr (exprToTermT env coerced) coerced
                    newEnv = extendEnv [(name, typeExpr)] env
                in interpret newEnv body

    -- Unit value
    ExprUnit -> ExprUnit

-- Helper function to get the type of an expression
exprToTermT :: Env -> Expr t -> TermT t
exprToTermT _ (ExprInt _) = SInt
exprToTermT _ (ExprDouble _) = SDouble
exprToTermT _ (ExprBool _) = SBool
exprToTermT _ (ExprString _) = SString
exprToTermT _ (ExprTuple _) = STuple
exprToTermT env (ExprIdent name) = case lookupEnv name env of
    Just (TypeExpr t _) -> unsafeCoerce t
    Nothing -> error $ "Unbound variable: " ++ unpack name
exprToTermT _ (ExprUnary _ _) = error "Cannot determine type of unary operation"
exprToTermT _ (ExprBinary _ _ _) = error "Cannot determine type of binary operation"
exprToTermT _ (ExprApp _ _) = error "Cannot determine type of function application"
exprToTermT env (ExprIf _ thenExpr _) = exprToTermT env thenExpr
exprToTermT _ (ExprWhile _ _) = SUnit
exprToTermT _ (ExprBlock _) = SUnit
exprToTermT _ ExprUnit = SUnit
exprToTermT env (ExprLet _ _ body) = exprToTermT env body 
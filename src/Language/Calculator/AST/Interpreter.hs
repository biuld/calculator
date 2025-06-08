{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Language.Calculator.AST.Interpreter (
    interpret
) where

import Language.Calculator.AST.Types
import Language.Calculator.AST.Env (Env, lookupEnv)
import Data.Text (Text, unpack)
import Data.Type.Equality ((:~:)(Refl), TestEquality(..))
import Unsafe.Coerce (unsafeCoerce)

-- Helper function to interpret SomeExpr
interpretSomeExpr :: Env -> SomeExpr -> SomeExpr
interpretSomeExpr env (SomeExpr e) = SomeExpr (interpret env e)

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
        Just (SomeExpr e) -> 
            -- We need to ensure the type matches
            case testEquality (typeOf e) (typeOf expr) of
                Just Refl -> unsafeCoerce e
                Nothing -> error $ "Type mismatch in variable lookup: " ++ unpack name
        Nothing -> error $ "Unbound variable: " ++ unpack name
    
    -- Tuple evaluation
    ExprTuple exprs -> ExprTuple $ map (interpretSomeExpr env) exprs
    
    -- Unary operations
    ExprUnary op e -> case (op, interpret env e) of
        (OpNot, ExprBool b) -> ExprBool $ not b
        _ -> error "Invalid unary operation"
    
    -- Binary operations
    ExprBinary op e1 e2 -> case (op, interpret env e1, interpret env e2) of
        -- Arithmetic operations for Double
        (OpPlus, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 + d2
        (OpMinus, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 - d2
        (OpMultiply, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 * d2
        (OpDivide, ExprDouble d1, ExprDouble d2) -> ExprDouble $ d1 / d2
        
        -- Arithmetic operations for Int
        (OpPlusInt, ExprInt i1, ExprInt i2) -> ExprInt $ i1 + i2
        (OpMinusInt, ExprInt i1, ExprInt i2) -> ExprInt $ i1 - i2
        (OpMultiplyInt, ExprInt i1, ExprInt i2) -> ExprInt $ i1 * i2
        (OpDivideInt, ExprInt i1, ExprInt i2) -> ExprInt $ i1 `div` i2
        
        -- Logical operations
        (OpAnd, ExprBool b1, ExprBool b2) -> ExprBool $ b1 && b2
        (OpOr, ExprBool b1, ExprBool b2) -> ExprBool $ b1 || b2
        
        -- Equality operations
        (OpEqualInt, ExprInt i1, ExprInt i2) -> ExprBool $ i1 == i2
        (OpNotEqualInt, ExprInt i1, ExprInt i2) -> ExprBool $ i1 /= i2
        (OpEqualDouble, ExprDouble d1, ExprDouble d2) -> ExprBool $ d1 == d2
        (OpNotEqualDouble, ExprDouble d1, ExprDouble d2) -> ExprBool $ d1 /= d2
        (OpEqualBool, ExprBool b1, ExprBool b2) -> ExprBool $ b1 == b2
        (OpNotEqualBool, ExprBool b1, ExprBool b2) -> ExprBool $ b1 /= b2
        (OpEqualString, ExprString s1, ExprString s2) -> ExprBool $ s1 == s2
        (OpNotEqualString, ExprString s1, ExprString s2) -> ExprBool $ s1 /= s2
        
        _ -> error "Invalid binary operation"
    
    -- Function application (simplified for now)
    ExprApp f args -> error $ "Function application not implemented: " ++ unpack f

-- Helper function to get the type of an expression
typeOf :: Expr t -> STy t
typeOf (ExprInt _) = SInt
typeOf (ExprDouble _) = SDouble
typeOf (ExprBool _) = SBool
typeOf (ExprString _) = SString
typeOf (ExprTuple _) = STuple
typeOf (ExprIdent _) = error "Cannot determine type of untyped identifier"
typeOf (ExprUnary _ _) = error "Cannot determine type of unary operation"
typeOf (ExprBinary _ _ _) = error "Cannot determine type of binary operation"
typeOf (ExprApp _ _) = error "Cannot determine type of function application" 
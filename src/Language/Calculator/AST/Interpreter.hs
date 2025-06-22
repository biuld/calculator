{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Interpreter (
    interpret
) where

import Language.Calculator.AST.Types
import Language.Calculator.AST.Values
import Data.Text (unpack)

-- Main interpreter function
interpret :: forall t. Env -> Expr t -> Value
interpret env expr = case expr of
    ExprLit (LitExpr {litBase = ExprBase _ SInt, litValue = val}) -> VInt val
    ExprLit (LitExpr {litBase = ExprBase _ SDouble, litValue = val}) -> VDouble val
    ExprLit (LitExpr {litBase = ExprBase _ SBool, litValue = val}) -> VBool val
    ExprLit (LitExpr {litBase = ExprBase _ SString, litValue = val}) -> VString val
    ExprLit (LitExpr {litBase = ExprBase _ STuple}) -> error "Tuple literal not directly supported, use ExprTuple"
    ExprLit (LitExpr {litBase = ExprBase _ SUnit}) -> VUnit
    ExprLit (LitExpr {litBase = ExprBase _ (SFun _ _)}) -> error "Function literal not supported"
    
    ExprIdent (IdentExpr {identName = name}) -> case lookupEnv name env of
        Just val -> val
        Nothing -> error $ "Unbound variable: " ++ unpack name
    
    ExprTuple (TupleExpr {tupleElems = exprs}) -> VTuple $ map (\(Exists e) -> interpret env e) exprs
    
    ExprUnary (UnaryExpr {unaryOp = op, unaryExpr = e}) -> case (op, interpret env e) of
        (NotBool, VBool b) -> VBool $ not b
        (PosDouble, VDouble d) -> VDouble d
        (NegDouble, VDouble d) -> VDouble $ -d
        (PosInt, VInt i) -> VInt i
        (NegInt, VInt i) -> VInt $ -i
        _ -> error "Invalid unary operation or type mismatch"
    
    ExprBinary (BinaryExpr {binaryOp = op, binaryLeft = e1, binaryRight = e2}) -> case (op, interpret env e1, interpret env e2) of
        (AddDouble, VDouble d1, VDouble d2) -> VDouble (d1 + d2)
        (SubDouble, VDouble d1, VDouble d2) -> VDouble (d1 - d2)
        (MulDouble, VDouble d1, VDouble d2) -> VDouble (d1 * d2)
        (DivDouble, VDouble d1, VDouble d2) -> VDouble (d1 / d2)
        (AddInt, VInt i1, VInt i2) -> VInt (i1 + i2)
        (SubInt, VInt i1, VInt i2) -> VInt (i1 - i2)
        (MulInt, VInt i1, VInt i2) -> VInt (i1 * i2)
        (DivInt, VInt i1, VInt i2) -> VInt (i1 `div` i2)
        (AndBool, VBool b1, VBool b2) -> VBool (b1 && b2)
        (OrBool, VBool b1, VBool b2) -> VBool (b1 || b2)
        (EqInt, VInt i1, VInt i2) -> VBool (i1 == i2)
        (NeInt, VInt i1, VInt i2) -> VBool (i1 /= i2)
        (EqDouble, VDouble d1, VDouble d2) -> VBool (d1 == d2)
        (NeDouble, VDouble d1, VDouble d2) -> VBool (d1 /= d2)
        (EqBool, VBool b1, VBool b2) -> VBool (b1 == b2)
        (NeBool, VBool b1, VBool b2) -> VBool (b1 /= b2)
        (EqString, VString s1, VString s2) -> VBool (s1 == s2)
        (NeString, VString s1, VString s2) -> VBool (s1 == s2)
        _ -> error "Invalid binary operation or type mismatch"

    ExprApp (AppExpr {appFun = f, appArg = a}) ->
        let funVal = interpret env f
            argVal = interpret env a
        in case funVal of
            VClosure param body closureEnv ->
                let newEnv = extendEnv [(param, argVal)] closureEnv
                in interpret newEnv body
            _ -> error "Attempted to apply a non-function value"

    ExprIf (IfExpr {ifCond = cond, ifThen = thenExpr, ifElse = elseExpr}) -> case interpret env cond of
        VBool True -> interpret env thenExpr
        VBool False -> interpret env elseExpr
        _ -> error "If condition must be a boolean"

    ExprWhile (WhileExpr {whileCond = cond, whileBody = body}) -> 
        let loop () = case interpret env cond of
                         VBool True -> interpret env body `seq` loop ()
                         VBool False -> VUnit
                         _ -> error "While condition must be a boolean"
        in loop ()

    ExprBlock (BlockExpr {blockElems = exprs}) -> 
        let evalBlock :: [Exists Expr] -> Env -> Value
            evalBlock [] _ = VUnit
            evalBlock [Exists e] env' = interpret env' e
            evalBlock (Exists e:es) env' = interpret env' e `seq` evalBlock es env'
        in evalBlock exprs env

    ExprLet (LetExpr {letBindings = bindings, letBody = body}) ->
        let newBindings = map (\(name, Exists value) -> (name, interpret env value)) bindings
            newEnv = extendEnv newBindings env
        in interpret newEnv body

    ExprLambda (LambdaExpr {lambdaParam = param, lambdaBody = body}) -> VClosure param body env
    
    ExprUnit (UnitExpr {}) -> VUnit
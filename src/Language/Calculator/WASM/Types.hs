module Language.Calculator.WASM.Types where

import Data.Int

data ValType = I32t | I64t | F32t | F64t deriving (Eq, Show)

data Val = I32v Int32 | I64v Int64 | F32v Float | F64v Double deriving (Eq, Show)

data Signage = Sign | UnSign deriving (Eq, Show)

data Inst where
    Const :: ValType -> Val -> Inst
    Equal :: ValType -> Val -> Val -> Inst
    NotEqual :: ValType -> Val -> Val -> Inst
    Gt :: ValType -> Signage -> Val -> Val -> Inst
    Ge :: ValType -> Signage -> Val -> Val -> Inst
    Lt :: ValType -> Signage -> Val -> Val -> Inst
    Le :: ValType -> Signage -> Val -> Val -> Inst
    Add :: ValType -> Val -> Val -> Inst
    Sub :: ValType -> Val -> Val -> Inst
    Mul :: ValType -> Val -> Val -> Inst
    Div :: ValType -> Signage -> Val -> Val -> Inst
    Rem :: ValType -> Signage -> Val -> Val -> Inst
    IfThenElse :: Val -> [Inst] -> [Inst] -> Inst
    Loop :: [Inst] -> Inst
    Local :: Id -> ValType -> Inst
    LocalGet :: Id -> Inst
    LocalSet :: Id -> Val -> Inst
    LocalTee :: Id -> Val -> Inst
    Global :: Id -> ValType -> Inst
    GlobalGet :: Id -> Inst
    GlobalSet :: Id -> Val -> Inst

deriving instance Show Inst
deriving instance Eq Inst

type Id = String

data Func = Func
    { id :: Id
    , ty :: ([ValType], [ValType])
    , body :: [Inst]
    }
    deriving (Eq, Show)

data Module = Module
    { id :: Maybe String
    , func :: [Func]
    , start :: Maybe String
    }
    deriving (Eq, Show)
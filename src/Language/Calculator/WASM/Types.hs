module Language.Calculator.WASM.Types where

import Data.Int

data ValType = I32t | I64t | F32t | F64t deriving (Eq, Show)

data Val = I32v Int32 | I64v Int64 | F32v Float | F64v Double deriving (Eq, Show)

data Inst where
    Const :: ValType -> Val -> Inst

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
    { id :: Maybe Id
    , func :: [Func]
    , start :: Maybe Id
    }
    deriving (Eq, Show)
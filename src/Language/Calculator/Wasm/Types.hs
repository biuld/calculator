module Language.Calculator.Wasm.Types (
    WatType(..),
    WatExpr(..),
    WatOp(..),
    WatModule(..),
    WatFunction(..),
    WatLit(..),
    WatTypeSignature(..)
) where

import Data.Text (Text)
import qualified Data.Map as Map

-- | Represents WASM value types
data WatType
  = I32    -- ^ 32-bit integer
  | I64    -- ^ 64-bit integer
  | F32    -- ^ 32-bit float
  | F64    -- ^ 64-bit float
  | Void   -- ^ Represents no value (for functions returning nothing)
  | WatTypeRef Text -- ^ Reference to a function (by name or index)
  deriving (Show, Eq, Ord)

-- | Represents WASM literal values
data WatLit
  = LitI32 Integer    -- ^ 32-bit integer literal
  | LitI64 Integer    -- ^ 64-bit integer literal
  | LitF32 Float    -- ^ 32-bit float literal
  | LitF64 Double   -- ^ 64-bit float literal
  deriving (Show, Eq)

-- | Represents WASM operations
data WatOp
  -- Integer operations
  = I32Add
  | I32Sub
  | I32Mul
  | I32DivS -- ^ Signed division
  | I32Eq
  | I32Ne
  | I32LtS  -- ^ Signed less than
  | I32GtS  -- ^ Signed greater than
  | I32And
  | I32Or
  | I32Xor

  -- Float operations
  | F32Add
  | F32Sub
  | F32Mul
  | F32Div
  | F32Eq
  | F32Ne
  | F32Lt
  | F32Gt

  -- Generic operations
  | LocalGet  -- ^ Get local variable
  | LocalSet  -- ^ Set local variable
  | LocalTee  -- ^ Set local variable and push value

  -- Control flow
  | If
  | Else
  | End
  | Loop
  | Block
  | Br    -- ^ Branch
  | BrIf  -- ^ Branch if true
  | Call  -- ^ Call function

  deriving (Show, Eq)

-- | Represents a WASM expression (instruction or sequence of instructions)
data WatExpr
  = WatConst WatLit      -- ^ WASM literal constant
  | WatOp WatOp [WatExpr]      -- ^ Operation with arguments
  | WatIf WatExpr [WatExpr] (Maybe [WatExpr]) -- ^ If expression: condition, then-branch, optional else-branch
  | WatBlock (Maybe Text) [WatExpr] -- ^ Block expression with optional label
  | WatLoop (Maybe Text) [WatExpr] -- ^ Loop expression with optional label
  | WatCall Text [WatExpr]       -- ^ Function call with name and arguments
  | WatLocalGet Text WatType      -- ^ Get local variable by name and type
  | WatLocalSet Text WatType      -- ^ Set local variable by name and type (value from stack)
  | WatLocalTee Text WatType      -- ^ Set local variable by name and push value (value from stack)
  | WatRef Text                 -- ^ Reference to a defined function

  deriving (Show, Eq)

-- Represents a WASM function type signature
data WatTypeSignature = WatTypeSignature
  { sigParams :: [WatType],
    sigResults :: [WatType]
  }
  deriving (Show, Eq, Ord)

-- | Represents a WASM function
data WatFunction = WatFunction
  { funcName :: Text,
    funcTypeAlias :: Text, -- Reference to a type signature alias in the module's type section
    funcParams :: [(Text, WatType)], -- New: Function parameters (name and type)
    funcLocals :: [(Text, WatType)],
    funcBody :: [WatExpr]
  }
  deriving (Show, Eq)

-- | Represents a complete WASM module
data WatModule = WatModule
  { moduleTypes :: Map.Map Text WatTypeSignature, -- New: Map of unique function type signatures with aliases
    moduleFunctions :: [WatFunction]
  -- Potentially add imports, exports, globals, data segments etc. later
  } deriving (Show, Eq)
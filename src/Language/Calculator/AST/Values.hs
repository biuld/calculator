{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Values (
    Value(..),
    Env(..),
    emptyEnv,
    extendEnv,
    lookupEnv
) where

import Language.Calculator.AST.Types (Expr)
import Data.Text (Text, unpack)
import qualified Data.Map as Map

data Env = Env {
    envBindings :: Map.Map Text Value,
    envParent   :: Maybe Env
}

data Value where
  VInt :: Integer -> Value
  VDouble :: Double -> Value
  VBool :: Bool -> Value
  VString :: Text -> Value
  VTuple :: [Value] -> Value
  VUnit :: Value
  VClosure :: Text -> Expr b -> Env -> Value

instance Show Value where
    show (VInt i) = show i
    show (VDouble d) = show d
    show (VBool b) = show b
    show (VString s) = unpack s
    show (VTuple vs) = "(" ++ unwords (map show vs) ++ ")"
    show VUnit = "()"
    show (VClosure p _ _) = "<closure " ++ unpack p ++ ">"


emptyEnv :: Env
emptyEnv = Env Map.empty Nothing

extendEnv :: [(Text, Value)] -> Env -> Env
extendEnv bindings env = Env (Map.fromList bindings) (Just env)

lookupEnv :: Text -> Env -> Maybe Value
lookupEnv name env = case Map.lookup name (envBindings env) of
    Just val -> Just val
    Nothing  -> case envParent env of
        Just parent -> lookupEnv name parent
        Nothing     -> Nothing 
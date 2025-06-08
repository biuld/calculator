{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Env (
    Env(..),
    emptyEnv,
    extendEnv,
    lookupEnv
) where

import Language.Calculator.AST.Types (SomeExpr)
import Data.Text (Text)
import qualified Data.Map as Map

-- Environment for storing variable values
data Env = Env {
    envBindings :: Map.Map Text SomeExpr,  -- 当前作用域的变量绑定
    envParent   :: Maybe Env               -- 父作用域
} deriving (Show)

-- Create an empty environment
emptyEnv :: Env
emptyEnv = Env Map.empty Nothing

-- Extend environment with new bindings
extendEnv :: [(Text, SomeExpr)] -> Env -> Env
extendEnv bindings env = Env (Map.fromList bindings) (Just env)

-- Lookup variable in environment
lookupEnv :: Text -> Env -> Maybe SomeExpr
lookupEnv name env = case Map.lookup name (envBindings env) of
    Just val -> Just val
    Nothing  -> case envParent env of
        Just parent -> lookupEnv name parent
        Nothing     -> Nothing 
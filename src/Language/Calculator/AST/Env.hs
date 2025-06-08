{-# LANGUAGE TypeFamilies #-}

module Language.Calculator.AST.Env (
    Env(..),
    emptyEnv,
    extendEnv,
    lookupEnv
) where

import Language.Calculator.AST.Types (TypeExpr)
import Data.Text (Text)
import qualified Data.Map as Map

-- Environment for storing variable values
data Env = Env {
    envBindings :: Map.Map Text TypeExpr,  -- Variable bindings in current scope
    envParent   :: Maybe Env                 -- Parent scope
} deriving (Show)

-- Create an empty environment
emptyEnv :: Env
emptyEnv = Env Map.empty Nothing

-- Extend environment with new bindings
extendEnv :: [(Text, TypeExpr)] -> Env -> Env
extendEnv bindings env = Env (Map.fromList bindings) (Just env)

-- Lookup variable in environment
lookupEnv :: Text -> Env -> Maybe TypeExpr
lookupEnv name env = case Map.lookup name (envBindings env) of
    Just val -> Just val
    Nothing  -> case envParent env of
        Just parent -> lookupEnv name parent
        Nothing     -> Nothing 
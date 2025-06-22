module Language.Calculator.Wasm.CompileEnv
  ( CompileEnv (..),
    initialCompileEnv,
    extractFuncLocals,
    addFunctionTypeSignature,
  )
where

import Data.Map qualified as Map
import Data.Text (Text, pack)
import Language.Calculator.Wasm.Types (WatType (..), WatTypeSignature (..))

-- | Represents the compilation environment
data CompileEnv = CompileEnv
  { localMap :: Map.Map Text (Either WatType Text),
    typeSignatures :: Map.Map WatTypeSignature Text -- Stores type signatures mapped to their aliases.
  }

-- | Initial compilation environment
initialCompileEnv :: CompileEnv
initialCompileEnv =
  CompileEnv
    { localMap = Map.empty,
      typeSignatures = Map.empty -- Initialize as empty Map.Map WatTypeSignature Text
    }

-- | Helper to extract actual local variables from the localMap
extractFuncLocals :: Map.Map Text (Either WatType Text) -> [(Text, WatType)]
extractFuncLocals =
  Map.foldrWithKey
    (\name val acc ->
        case val of
          Left watTy -> (name, watTy) : acc
          Right _ -> acc
    )
    []

-- | Helper to add a function type signature to the environment and get its alias
addFunctionTypeSignature :: CompileEnv -> Text -> WatTypeSignature -> (Text, CompileEnv)
addFunctionTypeSignature env funcName sig =
  case Map.lookup sig env.typeSignatures of
    Just alias -> (alias, env)
    Nothing ->
      let paramTypes = mconcat (pack . show <$> sig.sigParams)
          newAlias = "type_" <> funcName <> "_" <> paramTypes
          updatedTypeSignatures = Map.insert sig newAlias env.typeSignatures
          newEnv = env {typeSignatures = updatedTypeSignatures}
       in (newAlias, newEnv) 
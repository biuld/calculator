module Language.Calculator.Common.Types (
  SourceToken (..),
  SourceRange (..)
) where

import Text.Megaparsec (SourcePos, sourcePosPretty)

data SourceRange = SourceRange
  { srcStart :: SourcePos
  , srcEnd :: SourcePos
  }
  deriving (Eq)

instance Show SourceRange where
  show (SourceRange s e) = sourcePosPretty s <> " - " <> sourcePosPretty e

data SourceToken a = SourceToken
  { tokRange :: SourceRange
  , tokValue :: a
  }
  deriving (Eq, Show) 
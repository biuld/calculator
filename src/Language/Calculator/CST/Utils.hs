module Language.Calculator.CST.Utils (
  Parser,
  lexeme
) where

import Data.Text
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: forall a. Parser a -> Parser a
lexeme = L.lexeme skipSpace
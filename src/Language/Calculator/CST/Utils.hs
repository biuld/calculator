module Language.Calculator.CST.Utils (
  Parser,
  lexeme,
  unexpected,
  run
) where

import Data.List.NonEmpty qualified as NE
import Data.Text
import Data.Void
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Error (ErrorItem (Tokens))

type Parser = M.Parsec Void Text

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: forall a. Parser a -> Parser a
lexeme = L.lexeme skipSpace

unexpected :: forall a. String -> Parser a
unexpected s = M.unexpected $ Tokens (NE.fromList s)

run :: forall a. Parser a -> Text -> Either (M.ParseErrorBundle Text Void) a
run m = M.runParser m ""
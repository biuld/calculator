module Language.Calculator.CST.Utils (
  Parser,
  lexeme,
  unexpected,
  run,
  withPos,
  runTotal
) where

import Data.List.NonEmpty qualified as NE
import Data.Text
import Data.Void
import Language.Calculator.CST.Types
import Text.Megaparsec (getSourcePos)
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

lexeme :: forall a. Parser a -> Parser (SourceToken a)
lexeme = L.lexeme skipSpace . withPos

unexpected :: forall a. String -> Parser a
unexpected s = M.unexpected $ Tokens (NE.fromList s)

run :: forall a. Parser a -> Text -> Either (M.ParseErrorBundle Text Void) a
run m = M.runParser m ""

runTotal :: forall a. Parser a -> Text -> a
runTotal m input = case run m input of
  Right a -> a
  _ -> undefined


withPos :: forall a. Parser a -> Parser (SourceToken a)
withPos m = do
  start <- getSourcePos
  a <- m
  end <- getSourcePos
  return $ SourceToken{tokRange = SourceRange{srcStart = start, srcEnd = end}, tokValue = a}
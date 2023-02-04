module Language.Calculator.CST.Utils where
import Text.Megaparsec
import Data.Void
import Data.Text
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

type Parser = Parsec Void Text

skipSpace :: Parser ()
skipSpace =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme = L.lexeme skipSpace
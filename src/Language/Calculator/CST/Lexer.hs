module Language.Calculator.CST.Lexer (
  module Language.Calculator.CST.Lexer,
) where

import Data.Functor (void, ($>))
import Data.Text qualified as T
import Language.Calculator.CST.Types
import Language.Calculator.CST.Utils
import Text.Megaparsec (label, many, manyTill, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import Text.Megaparsec.Char.Lexer qualified as L

tokInteger :: Parser (SourceToken Integer)
tokInteger = label "integer" . lexeme $ L.decimal

tokDouble :: Parser (SourceToken Double)
tokDouble = label "double" . lexeme $ L.float

tokBool :: Parser (SourceToken Bool)
tokBool =
  label "boolean" . lexeme $ bool
 where
  bool = string "true" $> True <|> string "false" $> False

tokString :: Parser (SourceToken T.Text)
tokString = label "string" . lexeme $ str
 where
  str = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

tokIdent :: Parser (SourceToken T.Text)
tokIdent = label "identifier" . lexeme $ identifier
 where
  identifier :: Parser T.Text
  identifier =
    T.pack <$> do
      first <- letterChar <|> char '_'
      rest <- many alphaNumChar
      let ident = first : rest
      if ident `elem` keywords
        then unexpected ("keyword " <> ident)
        else return ident

tokOperator :: T.Text -> Parser (SourceToken Operator)
tokOperator t = lexeme $ string t

tokChar :: Char -> Parser ()
tokChar c = void . lexeme $ char c

keyword :: T.Text -> Parser ()
keyword s = void . lexeme $ string s

module Language.Calculator.CST.Lexer (
  module Language.Calculator.CST.Lexer,
) where

import Data.Functor (void, ($>))
import Data.Text qualified as T
import Language.Calculator.CST.Types
import Language.Calculator.CST.Utils
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Language.Calculator.Common.Types (SourceToken (..))

tokInteger :: Parser (SourceToken Integer)
tokInteger = M.label "integer" . lexeme $ L.decimal

tokDouble :: Parser (SourceToken Double)
tokDouble = M.label "double" . lexeme $ L.float

tokBool :: Parser (SourceToken Bool)
tokBool =
  M.label "boolean" . lexeme $ bool
 where
  bool = (string "true" $> True) M.<|> (string "false" $> False)

tokString :: Parser (SourceToken T.Text)
tokString = M.label "string" . lexeme $ str
 where
  str = char '"' >> T.pack <$> M.manyTill L.charLiteral (char '"')

tokIdent :: Parser (SourceToken T.Text)
tokIdent = M.label "identifier" . lexeme $ identifier
 where
  identifier :: Parser T.Text
  identifier =
    T.pack <$> do
      first <- letterChar M.<|> char '_'
      rest <- M.many alphaNumChar
      let ident = first : rest
      if ident `elem` keywords
        then unexpected ("keyword " <> ident)
        else return ident

tokOp :: Operator -> Parser (SourceToken Operator)
tokOp op = lexeme . M.label "operator" $ op <$ string (opToText op)

tokChar :: Char -> Parser ()
tokChar c = void . lexeme $ char c

keyword :: T.Text -> Parser ()
keyword s = void . lexeme $ string s

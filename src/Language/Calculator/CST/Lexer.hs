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


tokInteger :: Parser Integer
tokInteger = label "integer" . lexeme $ L.decimal

tokDouble :: Parser Double
tokDouble = label "double" . lexeme $ L.float

tokBool :: Parser Bool
tokBool =
  label "boolean" . lexeme $ bool
 where
  bool = string "true" $> True <|> string "false" $> False

tokString :: Parser T.Text
tokString = label "string" . lexeme $ str
 where
  str = char '"' >> T.pack <$> manyTill L.charLiteral (char '"')

tokIdent :: Parser T.Text
tokIdent = label "identifier" . lexeme $ identifier
 where
  identifier :: Parser T.Text
  identifier = T.pack <$> do
    first <- letterChar <|> char '_'
    rest <- many alphaNumChar
    let ident = first : rest
    if ident `elem` keywords
      then unexpected ("keyword " <> ident)
      else return ident

tokAdd :: Parser Operator
tokAdd = Add <$ lexeme (char '+')

tokSub :: Parser Operator
tokSub = Sub <$ lexeme (char '-')

tokDiv :: Parser Operator
tokDiv = Div <$ lexeme (char '/')

tokMul :: Parser Operator
tokMul = Mul <$ lexeme (char '*')

tokEqual :: Parser Operator
tokEqual = Equal <$ lexeme (string "==")

tokNotEqual :: Parser Operator
tokNotEqual = NotEqual <$ lexeme (string "!=")

tokAnd :: Parser Operator
tokAnd = And <$ lexeme (string "&&")

tokOr :: Parser Operator
tokOr = Or <$ lexeme (string "||")

tokNot :: Parser Operator
tokNot = Not <$ lexeme (char '!')

tokChar :: Char -> Parser ()
tokChar c = void . lexeme $ char c

keyword :: T.Text -> Parser ()
keyword s = void . lexeme $ string s

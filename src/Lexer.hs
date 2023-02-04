module Lexer where

import CST
import Data.Functor (void, ($>))
import Data.Text qualified as T
import Text.Megaparsec (choice, label, many, single, try, (<|>))
import Text.Megaparsec.Char (alphaNumChar, char, letterChar, string)
import Text.Megaparsec.Char.Lexer qualified as L
import Utils
import Data.Text

tokInteger :: Parser Integer
tokInteger = label "integer" L.decimal

tokDouble :: Parser Double
tokDouble = label "double" L.float

tokBool :: Parser Bool
tokBool =
  label "boolean" bool
 where
  bool = string "true" $> True <|> string "false" $> False

tokIdent :: Parser T.Text
tokIdent = label "identifier" $ do
  first <- letterChar <|> char '_'
  rest <- many alphaNumChar
  return . T.pack $ first : rest

tokAdd :: Parser Operator
tokAdd = Add <$ lexeme (char '+')

tokSub :: Parser Operator
tokSub = Sub <$ lexeme (char '-')

tokDiv :: Parser Operator
tokDiv = Div <$ lexeme (char '/')

tokMul :: Parser Operator
tokMul = Mul <$ lexeme (char '*')

tokEqual :: Parser Operator
tokEqual = Equal <$ lexeme (char '=')

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

keyword :: Text -> Parser ()
keyword s = void . lexeme $ string s

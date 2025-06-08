-- |
-- Module      : Language.Calculator.CST.Parser
-- Description : The parser for the calculator language
--
-- This module contains the parser for the calculator language.
-- It is implemented using the megaparsec library.
--
module Language.Calculator.CST.Parser (
    parseExpr,
    expr,
    term
) where

import qualified Control.Monad.Combinators.Expr as E
import Data.Text
import Data.Void
import Language.Calculator.CST.Lexer
import Language.Calculator.CST.Types
import Language.Calculator.CST.Utils
import Text.Megaparsec

-- A parser for parenthesized expressions, which can be a grouped expression,
-- an empty tuple, or a tuple with one or more elements.
pParen :: Parser Expr
pParen = parens $ do
    -- check for empty tuple "()"
    maybeFollowedByCloseParen <- optional (lookAhead (tokChar ')'))
    case maybeFollowedByCloseParen of
      Just _ -> return $ ExprTuple []
      Nothing -> do
        e <- expr
        isTuple <- optional (tokChar ',')
        case isTuple of
          -- Not a tuple, just a regular parenthesized expression like "(1+2)"
          Nothing -> return e
          -- A tuple with one or more elements, like "(a,)" or "(a,b,c)"
          Just _  -> do
              es <- sepEndBy expr (tokChar ',')
              return $ ExprTuple (e:es)

-- A more robust and maintainable expression parser using makeExprParser
-- This handles operator precedence and associativity correctly.
expr :: Parser Expr
expr = E.makeExprParser term table

term :: Parser Expr
term =
    choice
        [ pParen
        , ExprDouble <$> try tokDouble
        , ExprInt <$> tokInteger
        , ExprBool <$> tokBool
        , ExprString <$> tokString
        , try exprApp -- try is needed to distinguish from exprIdent
        , exprIdent
        ]

table :: [[E.Operator Parser Expr]]
table =
  [ [ E.InfixL (ExprBinary <$> tokOp OpAnd)
    , E.InfixL (ExprBinary <$> tokOp OpOr)
    ]
  , [ E.InfixL (ExprBinary <$> tokOp OpEqual)
    , E.InfixL (ExprBinary <$> tokOp OpNotEqual)
    ]
  , [ E.InfixL (ExprBinary <$> tokOp OpPlus)
    , E.InfixL (ExprBinary <$> tokOp OpMinus)
    ]
  , [ E.InfixL (ExprBinary <$> tokOp OpMultiply)
    , E.InfixL (ExprBinary <$> tokOp OpDivide)
    ]
  , [ E.Prefix (ExprUnary <$> tokOp OpPlus)
    , E.Prefix (ExprUnary <$> tokOp OpMinus)
    , E.Prefix (ExprUnary <$> tokOp OpNot)
    ]
  ]

exprApp :: Parser Expr
exprApp = do
    ident <- tokIdent
    ExprApp ident <$> tuple expr

exprIdent :: Parser Expr
exprIdent = ExprIdent <$> tokIdent

-- Helper for parenthesized expressions
parens :: Parser a -> Parser a
parens = wrapped '(' ')'

wrapped :: forall a. Char -> Char -> Parser a -> Parser a
wrapped open close m = do
    tokChar open
    e <- m
    tokChar close
    return e

-- contains zero, one, or more `m`
separated :: forall a b. Parser b -> Char -> Char -> Parser a -> Parser [a]
separated sep open close m = wrapped open close inner
  where
    inner = try rest <|> return []

    rest = do
        h <- m
        t <- many $ sep >> m
        return (h : t)

tuple :: Parser a -> Parser [a]
tuple = separated (tokChar ',') '(' ')'

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = run expr 
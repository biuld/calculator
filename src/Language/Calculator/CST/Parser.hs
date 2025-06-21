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
import Language.Calculator.Common.Types (SourceToken (..), SourceRange (..))

-- | Parses a parser `p` surrounded by `pOpen` and `pClose`,
--   and captures the SourceToken () of the entire construct.
withSourceRangeDelimited :: Parser () -> Parser () -> Parser a -> Parser (SourceToken (), a)
withSourceRangeDelimited pOpen pClose pContent = do
    startPos <- getSourcePos
    _ <- pOpen -- Discard the result of pOpen
    content <- pContent
    _ <- pClose -- Discard the result of pClose
    endPos <- getSourcePos
    return (SourceToken (SourceRange startPos endPos) (), content)

-- | A combinator that runs a parser `p` and captures its overall SourceToken ().
--   Useful for Expr constructors that take SourceToken () as their first argument.
--   Note: This helper is defined locally within each function that uses it because
--   the type of 'a' varies, and the constructor function's arity also varies.


-- A parser for parenthesized expressions, which can be a grouped expression,
-- an empty tuple, or a tuple with one or more elements.
pParen :: Parser Expr
pParen = do
    (sourceToken, result) <- parens $ do
        -- check for empty tuple "()"
        maybeFollowedByCloseParen <- optional (lookAhead (tokChar ')'))
        case maybeFollowedByCloseParen of
          Just _ -> return $ Left []
          Nothing -> do
            e <- expr
            isTuple <- optional (tokChar ',')
            case isTuple of
              -- Not a tuple, just a regular parenthesized expression like "(1+2)"
              Nothing -> return $ Right e
              -- A tuple with one or more elements, like "(a,)" or "(a,b,c)"
              Just _  -> do
                  es <- sepEndBy expr (tokChar ',')
                  return $ Left (e:es)
    case result of
      Left xs -> return $ ExprTuple sourceToken xs
      Right e -> return e

-- A more robust and maintainable expression parser using makeExprParser
-- This handles operator precedence and associativity correctly.
expr :: Parser Expr
expr = E.makeExprParser term table

term :: Parser Expr
term =
    choice
        [ pLet
        , pParen
        , pIf
        , pWhile
        , pBlock
        , pLambda
        , ExprDouble <$> try tokDouble
        , ExprInt <$> tokInteger
        , ExprBool <$> tokBool
        , ExprString <$> tokString
        , try exprApp -- try is needed to distinguish from exprIdent
        , exprIdent
        ]

-- Helper for Infix operators to capture SourceToken ()
infixOpWithSource :: SourceToken Operator -> Expr -> Expr -> Expr
infixOpWithSource opToken leftExpr rightExpr =
    let start = (getExprRange leftExpr).srcStart
        end = (getExprRange rightExpr).srcEnd
        range = SourceRange start end
    in ExprBinary (SourceToken range ()) opToken leftExpr rightExpr

-- Helper for Prefix operators to capture SourceToken ()
prefixOpWithSource :: SourceToken Operator -> Expr -> Expr
prefixOpWithSource opToken e =
    let start = (tokRange opToken).srcStart
        end = (getExprRange e).srcEnd
        range = SourceRange start end
    in ExprUnary (SourceToken range ()) opToken e

table :: [[E.Operator Parser Expr]]
table =
  [ [ E.InfixL (infixOpWithSource <$> tokOp OpAnd)
    , E.InfixL (infixOpWithSource <$> tokOp OpOr)
    ]
  , [ E.InfixL (infixOpWithSource <$> tokOp OpEqual)
    , E.InfixL (infixOpWithSource <$> tokOp OpNotEqual)
    ]
  , [ E.InfixL (infixOpWithSource <$> tokOp OpPlus)
    , E.InfixL (infixOpWithSource <$> tokOp OpMinus)
    ]
  , [ E.InfixL (infixOpWithSource <$> tokOp OpMultiply)
    , E.InfixL (infixOpWithSource <$> tokOp OpDivide)
    ]
  , [ E.Prefix (prefixOpWithSource <$> tokOp OpPlus)
    , E.Prefix (prefixOpWithSource <$> tokOp OpMinus)
    , E.Prefix (prefixOpWithSource <$> tokOp OpNot)
    ]
  ]

exprApp :: Parser Expr
exprApp = do
    startPos <- getSourcePos
    ident <- tokIdent
    (_, exprs) <- tuple expr
    endPos <- getSourcePos
    return $ ExprApp (SourceToken (SourceRange startPos endPos) ()) ident exprs

exprIdent :: Parser Expr
exprIdent = ExprIdent <$> tokIdent

-- Helper for parenthesized expressions
parens :: Parser a -> Parser (SourceToken (), a)
parens = withSourceRangeDelimited (tokChar '(') (tokChar ')')

-- contains zero, one, or more `m`
separated :: forall a. Char -> Char -> Char -> Parser a -> Parser (SourceToken (), [a])
separated sepChar openChar closeChar m =
    withSourceRangeDelimited (tokChar openChar) (tokChar closeChar) (sepBy m (tokChar sepChar))

tuple :: Parser a -> Parser (SourceToken (), [a])
tuple = separated ',' '(' ')'

-- Parse if-else statement
pIf :: Parser Expr
pIf = withExprSourceToken $ do
    keyword "if"
    cond <- expr
    keyword "then"
    thenExpr <- expr
    keyword "else"
    elseExpr <- expr
    return (cond, thenExpr, elseExpr)
  where
    -- Helper to create ExprIf with source token
    withExprSourceToken :: Parser (Expr, Expr, Expr) -> Parser Expr
    withExprSourceToken p = do
        startPos <- getSourcePos
        (cond, thenExpr, elseExpr) <- p
        endPos <- getSourcePos
        return $ ExprIf (SourceToken (SourceRange startPos endPos) ()) cond thenExpr elseExpr

-- Parse while statement
pWhile :: Parser Expr
pWhile = withExprSourceToken $ do
    keyword "while"
    cond <- expr
    keyword "do"
    body <- expr
    return (cond, body)
  where
    -- Helper to create ExprWhile with source token
    withExprSourceToken :: Parser (Expr, Expr) -> Parser Expr
    withExprSourceToken p = do
        startPos <- getSourcePos
        (cond, body) <- p
        endPos <- getSourcePos
        return $ ExprWhile (SourceToken (SourceRange startPos endPos) ()) cond body

-- Parse block of expressions
pBlock :: Parser Expr
pBlock = withExprSourceToken $ do
    tokChar '{'
    exprs <- sepEndBy expr (tokChar ';')
    tokChar '}'
    return exprs
  where
    -- Helper to create ExprBlock with source token
    withExprSourceToken :: Parser [Expr] -> Parser Expr
    withExprSourceToken p = do
        startPos <- getSourcePos
        exprs <- p
        endPos <- getSourcePos
        return $ ExprBlock (SourceToken (SourceRange startPos endPos) ()) exprs

-- Parse let expression
pLet :: Parser Expr
pLet = withExprSourceToken $ do
    keyword "let"
    bindings <- try parseMultipleBindings <|> parseSingleBinding
    keyword "in"
    body <- expr
    return (bindings, body)
  where
    -- Helper to create ExprLet with source token
    withExprSourceToken :: Parser ([(SourceToken Ident, Expr)], Expr) -> Parser Expr
    withExprSourceToken p = do
        startPos <- getSourcePos
        (bindings, body) <- p
        endPos <- getSourcePos
        return $ ExprLet (SourceToken (SourceRange startPos endPos) ()) bindings body

parseSingleBinding :: Parser [(SourceToken Ident, Expr)]
parseSingleBinding = (:[]) <$> parseBinding

parseMultipleBindings :: Parser [(SourceToken Ident, Expr)]
parseMultipleBindings = do
    tokChar '{'
    bindings <- sepEndBy parseBinding (tokChar ';')
    tokChar '}'
    return bindings

parseBinding :: Parser (SourceToken Ident, Expr)
parseBinding = do
    name <- tokIdent
    tokChar '='
    value <- expr
    return (name, value)

-- Parse lambda expression
pLambda :: Parser Expr
pLambda = withExprSourceToken $ do
    _ <- tokChar '\\'
    params <- some parseParam
    _ <- tokChar '-'
    _ <- tokChar '>'
    body <- expr
    return (params, body)
  where
    -- Helper to create ExprLambda with source token
    withExprSourceToken :: Parser ([(SourceToken Ident, Expr)], Expr) -> Parser Expr
    withExprSourceToken p = do
        startPos <- getSourcePos
        (params, body) <- p
        endPos <- getSourcePos
        return $ ExprLambda (SourceToken (SourceRange startPos endPos) ()) params body

    parseParam = do
        start <- getSourcePos
        param <- tokIdent
        ty <- optional $ do
            tokChar ':'
            ExprIdent <$> tokIdent
        end <- getSourcePos
        return (param, maybe (ExprIdent (SourceToken (SourceRange start end) ("Any" :: Text))) id ty)

parseExpr :: Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = run expr 
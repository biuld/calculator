module Parser where

import CST
import Lexer
import Text.Megaparsec
import Utils

exprAtom :: Parser Expr
exprAtom =
    lexeme $
        choice
            [ ExprInt <$> tokInteger
            , ExprDouble <$> try tokDouble
            , ExprBool <$> tokBool
            , ExprIdent <$> tokIdent
            ]

exprUnary :: Parser Expr
exprUnary = do
    op <- tokAdd <|> tokSub
    ExprUnary op <$> expr

exprBinary :: Parser Expr
exprBinary = do
    e1 <- expr
    op <- binOps
    ExprBinary op e1 <$> expr
  where
    binOps = tokAdd <|> tokSub <|> tokMul <|> tokDiv <|> tokAnd <|> tokOr <|> tokNot

wrapped :: forall a. Char -> Char -> Parser a -> Parser [a]
wrapped open close inner = do
    tokChar open
    es <- many inner
    tokChar close
    return es

tuple = wrapped '(' ')'

block = wrapped '{' '}'

exprTuple = ExprWrapped <$> tuple expr

exprBlock = ExprWrapped <$> block expr

exprIdent :: Parser Expr
exprIdent = ExprIdent <$> tokIdent

exprBind :: Parser Expr
exprBind = do
    keyword "let"
    ident <- tokIdent
    tokEqual
    ExprBind ident <$> expr

expr :: Parser Expr
expr = exprAtom <|> exprUnary <|> exprBinary <|> exprTuple <|> exprBlock <|> exprIdent <|> exprBind

stmAbs :: Parser Statement
stmAbs = do
    keyword "function"
    ident <- tokIdent
    params <- exprTuple
    StmAbs ident params <$> many stm

stmApp :: Parser Statement
stmApp = do
    ident <- exprIdent
    StmApp ident <$> exprTuple

stmIfElse :: Parser Statement
stmIfElse = do
    keyword "if"
    tokChar '('
    e <- expr
    tokChar ')'
    l <- block stm
    StmIfElse e l <$> r
  where
    r = (keyword "else" >> block stm) <|> return []

stmWhile :: Parser Statement
stmWhile = do
    keyword "while"
    tokChar '('
    e <- expr
    tokChar ')'
    StmWhile e <$> block stm

stmFor :: Parser Statement
stmFor = do
    keyword "for"
    tokChar '('
    init <- expr
    tokChar ';'
    condition <- expr
    tokChar ';'
    increment <- expr
    tokChar ')'
    StmFor init condition increment <$> block stm

stmE :: Parser Statement
stmE = StmE <$> expr

stm :: Parser Statement
stm = stmAbs <|> stmApp <|> stmIfElse <|> stmWhile <|> stmFor <|> stmE
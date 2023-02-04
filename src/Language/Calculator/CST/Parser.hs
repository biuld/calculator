module Language.Calculator.CST.Parser (stm) where

import Language.Calculator.CST.Lexer
import Language.Calculator.CST.Types
import Language.Calculator.CST.Utils
import Text.Megaparsec

exprApp :: Parser Expr
exprApp = do
    ident <- exprIdent
    ExprApp ident <$> tuple expr

exprAtom :: Parser Expr
exprAtom =
    choice
        [ ExprInt <$> tokInteger
        , ExprDouble <$> try tokDouble
        , ExprBool <$> tokBool
        , ExprString <$> tokString
        , exprIdent
        , exprApp
        ]

exprUnary :: Parser Expr
exprUnary = do
    op <- tokAdd <|> tokSub
    ExprUnary op <$> expr

exprBinary :: Parser Expr
exprBinary =
    pth <|> do
        l <- literal
        op <- binOps
        r <- literal
        rest op l r <|> return (ExprBinary op l r)
  where
    rest op l r = do
        op' <- binOps
        e <- expr
        if op `ge` op'
            then return $ ExprBinary op' (ExprBinary op l r) e
            else return $ ExprBinary op l (ExprBinary op' r e)

    binOps = tokAdd <|> tokSub <|> tokMul <|> tokDiv <|> tokAnd <|> tokOr <|> tokNot <|> tokEqual <|> tokNotEqual

    pth = wrapped '(' ')' expr

    literal = exprAtom <|> pth

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

block :: Parser a -> Parser [a]
block = separated (tokChar ';') '{' '}'

-- as least contains two elements
exprTuple :: Parser Expr
exprTuple = ExprTuple <$> wrapped '(' ')' inner
  where
    inner = do
        h <- expr
        t <- some $ tokChar ',' >> expr
        return (h : t)

exprIdent :: Parser Expr
exprIdent = ExprIdent <$> try tokIdent

exprBind :: Parser Expr
exprBind = do
    keyword "let"
    ident <- tokIdent
    tokChar '='
    ExprBind ident <$> expr

expr :: Parser Expr
expr = exprUnary <|> try exprBinary <|> try exprTuple <|> exprApp <|> exprIdent <|> exprBind <|> exprAtom

stmBlock :: Parser [Statement]
stmBlock = block stm

stmAbs :: Parser Statement
stmAbs = do
    keyword "function"
    ident <- tokIdent
    params <- tuple exprIdent
    StmAbs ident params <$> stmBlock

stmIfElse :: Parser Statement
stmIfElse = do
    keyword "if"
    e <- wrapped '(' ')' expr
    l <- stmBlock
    StmIfElse e l <$> r
  where
    r = (keyword "else" >> stmBlock) <|> return []

stmWhile :: Parser Statement
stmWhile = do
    keyword "while"
    e <- wrapped '(' ')' expr
    StmWhile e <$> stmBlock

stmFor :: Parser Statement
stmFor = do
    keyword "for"
    tokChar '('
    initE <- expr
    tokChar ';'
    condition <- expr
    tokChar ';'
    increment <- expr
    tokChar ')'
    StmFor initE condition increment <$> stmBlock

stmE :: Parser Statement
stmE = StmE <$> expr

stm :: Parser Statement
stm = stmAbs <|> stmIfElse <|> stmWhile <|> stmFor <|> stmE
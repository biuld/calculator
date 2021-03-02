module Parser where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.State.Strict (MonadState (get, put), State, modify)
import Lexer

data Expr a
  = Figure Int
  | Boolean Bool
  | Pth (Expr a)
  | Unary Token (Expr a)
  | Binary Token (Expr a) (Expr a)

instance Show (Expr a) where
  show (Figure i) = show i
  show (Boolean b) = show b
  show (Pth e) = "( " <> show e <> " )"
  show (Binary op l r) = show l <> " " <> show op <> " " <> show r
  show (Unary op e) = show op <> " " <> show e

type Precedence = Int

getUnaryOpPrecedence :: Token -> Precedence
getUnaryOpPrecedence Add = 6
getUnaryOpPrecedence Sub = 6
getUnaryOpPrecedence Not = 6
getUnaryOpPrecedence _ = 0

getBinaryOpPrecedence :: Token -> Precedence
getBinaryOpPrecedence Mul = 5
getBinaryOpPrecedence Div = 5
getBinaryOpPrecedence Add = 4
getBinaryOpPrecedence Sub = 4
getBinaryOpPrecedence Equal = 3
getBinaryOpPrecedence NotEqual = 3
getBinaryOpPrecedence And = 2
getBinaryOpPrecedence Or = 1
getBinaryOpPrecedence _ = 0

type Parser = ExceptT String (State [Token])

parse :: Parser (Expr a)
parse = do
  e <- parseExpr 0
  t <- get
  tryRestart (e, t)
  where
    tryRestart :: (Expr a, [Token]) -> Parser (Expr a)
    tryRestart (l, []) = return l
    tryRestart (l, op : tail) = do
      put tail
      r <- parseExpr (getBinaryOpPrecedence op)
      t <- get
      case (r, t) of
        (r, []) -> return $ Binary op l r
        (r, _) -> tryRestart (Binary op l r, t)

    parseExpr :: Precedence -> Parser (Expr a)
    parseExpr p = do
      t <- get
      case t of
        (Add : _) -> parseUnary p
        (Sub : _) -> parseUnary p
        (Not : _) -> parseUnary p
        _ -> parseBinary p

    parseBinary :: Precedence -> Parser (Expr a)
    parseBinary p = do
      e <- parsePth
      t <- get
      case (e, t) of
        (l, []) -> return l
        (l, op : tail) ->
          let precedence = getBinaryOpPrecedence op
           in if precedence > p
                then do
                  put tail
                  r <- parseExpr precedence
                  return $ Binary op l r
                else return l

    parseUnary :: Precedence -> Parser (Expr a)
    parseUnary p = do
      t <- get
      let (operator : tail) = t
          precedence = getBinaryOpPrecedence operator
       in if precedence >= p
            then do
              put tail
              operand <- parseExpr precedence
              return $ Unary operator operand
            else throwError $ "Error token " <> show operator

    parsePth :: Parser (Expr a)
    parsePth = do
      t <- get
      case t of
        (OpenPth : tail) -> do
          put tail
          e <- parseExpr 0
          modify (drop 1) -- drop ClosePth
          return $ Pth e
        _ -> parseLiteral

    parseLiteral :: Parser (Expr a)
    parseLiteral = do
      t <- get
      case t of
        ((I i) : tail) -> do put tail; return $ Figure i
        ((B b) : tail) -> do put tail; return $ Boolean b
        (h : _) -> throwError $ "expected literal expression, got " <> show h
        [] -> throwError "expected literal expression, got nothing"
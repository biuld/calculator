module Parser where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState (get, put), State, evalState)
import Lexer
import Utils

data Expr
  = Figure Int
  | Boolean Bool
  | Unit
  | Pth Expr
  | Unary Token Expr
  | Binary Token Expr Expr
  | If Expr Expr Expr
  deriving (Eq, Show)

instance Display Expr where
  disp (Figure i) = show i <> " :: Figure"
  disp (Boolean b) = show b <> " :: Boolean"
  disp (Pth e) = "( " <> disp e <> " )"
  disp (Binary op l r) = disp l <> " " <> disp op <> " " <> disp r
  disp (Unary op e) = disp op <> " " <> disp e
  disp Unit = "()"

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

parse :: [Token] -> Either String Expr
parse t = evalState (runExceptT $ parseExpr 0) t
  where
    parseExpr :: Precedence -> Parser Expr
    parseExpr p = do
      t <- get
      case t of
        (Add : _) -> parseUnary p
        (Sub : _) -> parseUnary p
        (Not : _) -> parseUnary p
        (Ift : _) -> parseIf
        _ -> parseBinary p

    parseIf :: Parser Expr
    parseIf = do
      t <- get
      case t of
        (Ift : tail) -> do
          put tail
          b <- parseExpr 0
          l <- parseExpr 0
          t' <- get
          case t' of
            (Elt : tail') -> do
              put tail'
              r <- parseExpr 0
              return $ If b l r
            (h : _) -> throwError $ "expected else token, got " <> disp h
            [] -> return $ If b l Unit

    parseBinary :: Precedence -> Parser Expr
    parseBinary p = do
      e <- parsePth
      t <- get
      loop (p, e, t)
      where
        loop :: (Precedence, Expr, [Token]) -> Parser Expr
        loop (_, e, []) = return e
        loop (p, l, op : tail) =
          let p' = getBinaryOpPrecedence op
           in if p' > p
                then do
                  put tail
                  r <- parseExpr p'
                  t' <- get
                  loop (0, Binary op l r, t')
                else return l

    parseUnary :: Precedence -> Parser Expr
    parseUnary p = do
      t <- get
      let (operator : tail) = t
          precedence = getUnaryOpPrecedence operator
       in if precedence >= p
            then do
              put tail
              operand <- parseExpr precedence
              return $ Unary operator operand
            else throwError $ "Error token " <> disp operator

    parsePth :: Parser Expr
    parsePth = do
      t <- get
      case t of
        (OpenPth : tail) -> do
          put tail
          e <- parseExpr 0
          t' <- get
          case t' of
            (ClosePth : tail) -> do put tail; return e
            [] -> throwError "expected ')', got nothing"
        _ -> parseLiteral

    parseLiteral :: Parser Expr
    parseLiteral = do
      t <- get
      case t of
        ((I i) : tail) -> do put tail; return $ Figure i
        ((B b) : tail) -> do put tail; return $ Boolean b
        (h : _) -> throwError $ "expected literal expression, got " <> disp h
        [] -> throwError "expected literal expression, got nothing"
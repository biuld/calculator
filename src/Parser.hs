{-# LANGUAGE TemplateHaskell #-}

module Parser where

import Control.Applicative
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState (get, put), State, runState)
import Data.List (intercalate)
import Data.Map.Strict
import Lexer
import Optics
import Utils

data Expr
  = Figure Int
  | Boolean Bool
  | Unit
  | Pth Expr
  | Unary Token Expr
  | Binary Token Expr Expr
  | If Expr Expr Expr
  | Bind String Expr
  | Name String
  | Tuple [Expr]
  deriving (Eq, Show)

instance Display Expr where
  disp (Figure i) = show i <> " :: Figure"
  disp (Boolean b) = show b <> " :: Boolean"
  disp (Pth e) = "( " <> disp e <> " )"
  disp (Binary op l r) = disp l <> " " <> disp op <> " " <> disp r
  disp (Unary op e) = disp op <> " " <> disp e
  disp Unit = "()"
  disp (Name t) = t
  disp (Tuple es) = "(" <> intercalate ", " (fmap disp es) <> ")"
  disp other = show other

type Precedence = Int

unOps = [Add, Sub, Not]

getUnaryOpPrecedence :: Token -> Precedence
getUnaryOpPrecedence Add = 6
getUnaryOpPrecedence Sub = 6
getUnaryOpPrecedence Not = 6
getUnaryOpPrecedence _ = 0

binOps = [Add, Sub, Mul, Div, And, Or, Equal, NotEqual]

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

data Context = Context
  { _tokens :: [Token],
    _names :: Map String Expr,
    _tree :: Expr -- the syntax tree
  }
  deriving (Eq, Show)

makeLenses ''Context

emptyContext :: Context
emptyContext = Context [] Data.Map.Strict.empty Unit

parse :: Pack Context Expr
parse = do
  e <- parseExpr 0
  c <- get
  put (c & tree .~ e) -- for eval's further use
  return e

parseExpr :: Precedence -> Pack Context Expr
parseExpr p = parseUnary p <|> parseIf <|> parseBind <|> parseBinary p

parseIf :: Pack Context Expr
parseIf = do
  c <- get
  case c ^. tokens of
    (Ift : tail) -> do
      put (c & tokens .~ tail)
      b <- parseExpr 0
      l <- parseExpr 0
      c' <- get
      case c' ^. tokens of
        (Elt : tail') -> do
          put (c' & tokens .~ tail')
          r <- parseExpr 0
          return $ If b l r
        [] -> return $ If b l Unit
        (h : _) -> throwError $ "expected else token, got " <> disp h
    _ -> throwError ""

parseBind :: Pack Context Expr
parseBind = do
  c@Context {_tokens = t, _names = n} <- get
  case t of
    (Let : N name : Assign : tail) -> do
      put (c & tokens .~ tail)
      e <- parseExpr 0
      put (c & names .~ insert name e n)
      return $ Bind name e
    _ -> throwError ""

parseUnary :: Precedence -> Pack Context Expr
parseUnary p = do
  c <- get
  let (operator : tail) = c ^. tokens
      precedence = getUnaryOpPrecedence operator
   in if operator `elem` unOps && precedence >= p
        then do
          put (c & tokens .~ tail)
          operand <- parseExpr precedence
          return $ Unary operator operand
        else throwError $ "Error token " <> disp operator

parseBinary :: Precedence -> Pack Context Expr
parseBinary p = do
  e <- parsePth
  c <- get
  loop (p, e, c)
  where
    loop :: (Precedence, Expr, Context) -> Pack Context Expr
    loop (_, e, Context {_tokens = []}) = return e
    loop (p, l, c@Context {_tokens = op : tail})
      | op `elem` binOps =
        let p' = getBinaryOpPrecedence op
         in if p' > p
              then do
                put (c & tokens .~ tail)
                r <- parseExpr p'
                c' <- get
                loop (0, Binary op l r, c')
              else return l
      | otherwise = return l

parsePth :: Pack Context Expr
parsePth = do
  c <- get
  case c ^. tokens of
    (OpenPth : tail) -> do
      put (c & tokens .~ tail)
      e <- parseExpr 0
      c' <- get
      case c' ^. tokens of
        (ClosePth : tail') -> do put (c' & tokens .~ tail'); return e
        [] -> throwError "expected ')', got nothing"
        (CommaSep : _) -> parseTuple (e, c')
        (h : _) -> throwError $ "expected ')', got " <> disp h
    _ -> parseLiteral
  where
    parseTuple :: (Expr, Context) -> Pack Context Expr
    parseTuple (e, c@Context {_tokens = []}) = return e
    parseTuple (e, c@Context {_tokens = h : tail}) =
      case h of
        CommaSep -> do
          put (c & tokens .~ tail)
          next <- parseExpr 0
          c' <- get
          parseTuple (Tuple [e, next], c')
        _ -> return e

parseLiteral :: Pack Context Expr
parseLiteral = do
  c <- get
  case c ^. tokens of
    ((I i) : tail) -> do put (c & tokens .~ tail); return $ Figure i
    ((B b) : tail) -> do put (c & tokens .~ tail); return $ Boolean b
    ((N n) : tail) -> do put (c & tokens .~ tail); return $ Name n
    (h : _) -> throwError $ "expected literal expression, got " <> disp h
    [] -> throwError "expected literal expression, got nothing"
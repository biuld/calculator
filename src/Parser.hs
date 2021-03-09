module Parser where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.State.Strict (MonadState (get, put), State, runState)
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
  deriving (Eq, Show)

instance Display Expr where
  disp (Figure i) = show i <> " :: Figure"
  disp (Boolean b) = show b <> " :: Boolean"
  disp (Pth e) = "( " <> disp e <> " )"
  disp (Binary op l r) = disp l <> " " <> disp op <> " " <> disp r
  disp (Unary op e) = disp op <> " " <> disp e
  disp Unit = "()"
  disp (Name t) = t

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

type Context = ([Token], Map String Expr)

type Parser = ExceptT String (State Context)

parse :: [Token] -> Map String Expr -> (Either String Expr, Map String Expr)
parse t n =
  let (e, (_, n')) = runState (runExceptT $ parseExpr 0) (t, n) in (e, n')
  where
    parseExpr :: Precedence -> Parser Expr
    parseExpr p = do
      c <- get
      case view _1 c of
        (Add : _) -> parseUnary p
        (Sub : _) -> parseUnary p
        (Not : _) -> parseUnary p
        (Ift : _) -> parseIf
        (Let : _) -> parseBind
        _ -> parseBinary p

    parseIf :: Parser Expr
    parseIf = do
      c <- get
      case view _1 c of
        (Ift : tail) -> do
          put (set _1 tail c)
          b <- parseExpr 0
          l <- parseExpr 0
          c' <- get
          case view _1 c' of
            (Elt : tail') -> do
              put (set _1 tail' c')
              r <- parseExpr 0
              return $ If b l r
            [] -> return $ If b l Unit
            (h : _) -> throwError $ "expected else token, got " <> disp h

    parseBind :: Parser Expr
    parseBind = do
      c@(t, n) <- get
      case t of
        (Let : N name : Assign : tail) -> do
          put (set _1 tail c)
          e <- parseExpr 0
          put (set _2 (insert name e n) c)
          return $ Bind name e
        other -> throwError "illegal let expression"

    parseBinary :: Precedence -> Parser Expr
    parseBinary p = do
      e <- parsePth
      t <- get
      loop (p, e, t)
      where
        loop :: (Precedence, Expr, Context) -> Parser Expr
        loop (_, e, ([], _)) = return e
        loop (p, l, c@(op : tail, _)) =
          let p' = getBinaryOpPrecedence op
           in if p' > p
                then do
                  put (set _1 tail c)
                  r <- parseExpr p'
                  t' <- get
                  loop (0, Binary op l r, t')
                else return l

    parseUnary :: Precedence -> Parser Expr
    parseUnary p = do
      c <- get
      let (operator : tail) = view _1 c
          precedence = getUnaryOpPrecedence operator
       in if precedence >= p
            then do
              put (set _1 tail c)
              operand <- parseExpr precedence
              return $ Unary operator operand
            else throwError $ "Error token " <> disp operator

    parsePth :: Parser Expr
    parsePth = do
      c <- get
      case view _1 c of
        (OpenPth : tail) -> do
          put (set _1 tail c)
          e <- parseExpr 0
          c' <- get
          case view _1 c' of
            (ClosePth : tail) -> do put (set _1 tail c'); return e
            [] -> throwError "expected ')', got nothing"
        _ -> parseLiteral

    parseLiteral :: Parser Expr
    parseLiteral = do
      c <- get
      case view _1 c of
        ((I i) : tail) -> do put (set _1 tail c); return $ Figure i
        ((B b) : tail) -> do put (set _1 tail c); return $ Boolean b
        ((N n) : tail) -> do put (set _1 tail c); return $ Name n
        (h : _) -> throwError $ "expected literal expression, got " <> disp h
        [] -> throwError "expected literal expression, got nothing"
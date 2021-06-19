module Parser where

import Common
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (intercalate)
import Optics

parse :: Pack Context ()
parse = do
  e <- parseExpr 0
  c <- get
  put (c & tree .~ e & value .~ e)

parseF :: Pack Context ()
parseF = do
  c <- get
  loop c []
  where
    loop :: Context -> [Expr] -> Pack Context ()
    loop c es = case c ^. tokens of
      [] -> let e = Group es in put (c & tree .~ e & value .~ e)
      _ -> do
        e <- parseExpr 0
        c <- get
        loop c (es ++ [e])

parseExpr :: Precedence -> Pack Context Expr
parseExpr p =
  parseUnary p
    <|> parseIf
    <|> parseBind
    <|> parseFuncCall
    <|> parseFuncDef
    <|> parseBinary p

parseIf :: Pack Context Expr
parseIf = do
  c <- get
  case c ^. tokens of
    (Ift : tail) -> do
      restore tail
      b <- parseExpr 0
      l <- parseExpr 0
      c' <- get
      case c' ^. tokens of
        (Elt : tail') -> do
          restore tail'
          r <- parseExpr 0
          return $ If b l r
        [] -> return $ If b l Unit
        (h : _) -> throwError $ "expected else token, got " <> disp h
    _ -> throwError ""

parseBind :: Pack Context Expr
parseBind = do
  c <- get
  case c ^. tokens of
    (Let : N name : Assign : tail) -> do
      restore tail
      e <- parseExpr 0
      return $ Bind name e
    _ -> throwError ""

parseFuncDef :: Pack Context Expr
parseFuncDef = do
  c <- get
  case c ^. tokens of
    t@(Def : N name : OpenPth : N p : tail) -> do
      restore tail
      ps <- eatStr [p] t
      FuncDef name ps <$> getBody
    _ -> throwError ""
  where
    eatStr :: [String] -> [Token] -> Pack Context [String]
    eatStr ps t = do
      c <- get
      case c ^. tokens of
        (CommaSep : tail) -> restore tail >> eatStr ps t
        (N n : tail) -> restore tail >> eatStr (ps ++ [n]) t
        (ClosePth : tail) -> restore tail >> return ps
        (h : _) ->
          restore t >> throwError ("illegal function parameter " <> disp h)
        [] -> restore t >> throwError "illegal eof"

    getBody = do
      body <- parseBlock
      case body of
        Group b -> return b
        other -> return [other]

parseFuncCall :: Pack Context Expr
parseFuncCall = do
  c <- get
  case c ^. tokens of
    (N name : tail@(OpenPth : _)) -> do
      restore tail
      param <- parsePth
      case param of
        Group p -> return $ FuncCall name p
        other -> return $ FuncCall name [other]
    _ -> throwError ""

parseUnary :: Precedence -> Pack Context Expr
parseUnary p = do
  c <- get
  case c ^. tokens of
    (op : tail) | op `elem` unOps -> do
      let precedence = getUnaryOpPrecedence op
       in if precedence >= p
            then do
              restore tail
              operand <- parseExpr precedence
              return $ Unary op operand
            else throwError $ "Error token " <> disp op
    _ -> throwError ""

parseBinary :: Precedence -> Pack Context Expr
parseBinary p = do
  e <- parsePth <|> parseLiteral
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
                restore tail
                r <- parseExpr p'
                c' <- get
                loop (0, Binary op l r, c')
              else return l
      | otherwise = return l

parseGroup :: Token -> Token -> Token -> Pack Context Expr
parseGroup open close sep = do
  c <- get
  case c ^. tokens of
    (open' : tail) | open' == open -> do
      restore tail
      e <- parseExpr 0
      c' <- get
      group (e, c')
    _ -> throwError ""
  where
    group :: (Expr, Context) -> Pack Context Expr
    group (e, c@Context {_tokens = []}) = throwError $ "expected '" <> disp close <> "', got nothing"
    group (e, c@Context {_tokens = h : tail})
      | h == sep = do
        restore tail
        next <- parseExpr 0
        c' <- get
        group (Group [e, next], c')
      | h == close = restore tail >> return e
      | otherwise = case open of
        OpenBracket -> throwError $ "try to parse a Block, got " <> disp h
        OpenPth -> throwError $ "try to parse a Tuple, got " <> disp h
        _ -> throwError $ "try to parse a Group, got " <> disp h

parsePth = parseGroup OpenPth ClosePth CommaSep

parseBlock = parseGroup OpenBracket CloseBracket LineSep

parseLiteral :: Pack Context Expr
parseLiteral = do
  c <- get
  case c ^. tokens of
    ((I i) : tail) -> restore tail >> return (Figure i)
    ((B b) : tail) -> restore tail >> return (Boolean b)
    ((N n) : tail) -> restore tail >> return (Name n)
    (h : _) -> throwError $ "expected literal expression, got " <> disp h
    [] -> throwError "expected literal expression, got nothing"
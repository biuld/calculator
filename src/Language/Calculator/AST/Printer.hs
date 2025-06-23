module Language.Calculator.AST.Printer (pprint) where

import Language.Calculator.AST.Types
import Language.Calculator.Common.Printer
import Data.Text (unpack)

pprintExpr :: String -> Bool -> Exists Expr -> IO ()
pprintExpr prefix isLast (Exists e) =
    let
      (sign, childPrefix) = if isLast
        then (end, prefix <> space)
        else (start, prefix <> vertical)
    in case e of
    ExprLit (LitExpr {litValue = val}) -> putStrLn $ prefix <> sign <> show val
    ExprIdent (IdentExpr {identName = i}) -> putStrLn $ prefix <> sign <> unpack i
    ExprTuple (TupleExpr {tupleElems = es}) ->
        putStrLn (prefix <> sign <> "()") >> go pprintExpr childPrefix es
    ExprUnary (UnaryExpr {unaryOp = op, unaryExpr = e1}) ->
        putStrLn (prefix <> sign <> show op) >> pprintExpr childPrefix True (Exists e1)
    ExprBinary (BinaryExpr {binaryOp = op, binaryLeft = e1, binaryRight = e2}) ->
        putStrLn (prefix <> sign <> show op) >> 
        pprintExpr childPrefix False (Exists e1) >> 
        pprintExpr childPrefix True (Exists e2)
    ExprApp (AppExpr {appFun = f, appArg = a}) ->
        putStrLn (prefix <> sign <> "APP") >>
        pprintExpr childPrefix False (Exists f) >>
        pprintExpr childPrefix True (Exists a)
    ExprIf (IfExpr {ifCond = cond, ifThen = thenExpr, ifElse = elseExpr}) ->
        putStrLn (prefix <> sign <> "IF") >>
        pprintExpr childPrefix False (Exists cond) >>
        putStrLn (childPrefix <> start <> "THEN") >>
        pprintExpr (childPrefix <> vertical) True (Exists thenExpr) >>
        putStrLn (childPrefix <> end <> "ELSE") >>
        pprintExpr (childPrefix <> space) True (Exists elseExpr)
    ExprWhile (WhileExpr {whileCond = cond, whileBody = body}) ->
        putStrLn (prefix <> sign <> "WHILE") >>
        pprintExpr childPrefix False (Exists cond) >>
        putStrLn (childPrefix <> end <> "DO") >>
        pprintExpr (childPrefix <> space) True (Exists body)
    ExprBlock (BlockExpr {blockElems = es}) ->
        putStrLn (prefix <> sign <> "BLOCK") >> go pprintExpr childPrefix es
    ExprUnit (UnitExpr {}) ->
        putStrLn $ prefix <> sign <> "UNIT"
    ExprLet (LetExpr {letBindings = bindings, letBody = body}) -> do
        putStrLn (prefix <> sign <> "LET")
        let printBinding (name, value) = do
                putStrLn (childPrefix <> start <> unpack name)
                pprintExpr (childPrefix <> vertical) True value
        mapM_ printBinding bindings
        putStrLn (childPrefix <> end <> "IN")
        pprintExpr (childPrefix <> space) True (Exists body)
    ExprLambda (LambdaExpr {lambdaParam = param, lambdaParamType = ty, lambdaBody = body}) ->
        putStrLn (prefix <> sign <> "LAMBDA " <> unpack param <> " : " <> show ty) >>
        pprintExpr childPrefix True (Exists body)

pprint :: Exists Expr -> IO ()
pprint = pprintExpr "" True 
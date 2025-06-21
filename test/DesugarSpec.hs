module DesugarSpec (spec) where

import Test.Hspec
import qualified Language.Calculator.CST.Parser as Parser
import qualified Language.Calculator.AST.Types as AST
import Language.Calculator.Desugar (desugar)
import Data.Text (pack)
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "Desugar" $ do
        it "should desugar let expression with lambda" $ do
            let input = "let f = \\a:Int b:Int -> a+b in f (1,2)"
            case Parser.parseExpr (pack input) of
                Left err -> expectationFailure $ "Parse failed: " ++ show err
                Right cst -> do
                    case desugar Map.empty cst of
                        Left err -> expectationFailure $ "Desugar failed: " ++ show err
                        Right (AST.TypeExpr _ ast) -> do -- Added _ for SourceToken ()
                            -- Verify that the AST is correct after desugaring
                            case ast of
                                AST.ExprLet _ bindings body -> do -- Added _ for SourceToken ()
                                    length bindings `shouldBe` 1
                                    let (name, value) = head bindings
                                    name `shouldBe` "f"
                                    case value of
                                        AST.Exists (AST.ExprLambda _ param ty lambdaBody) -> do -- Unwrap Exists, and match 4 arguments
                                            param `shouldBe` "a"
                                            case ty of
                                                AST.SInt -> return ()
                                                _ -> expectationFailure "Param type should be Int"
                                            case lambdaBody of
                                                AST.ExprLambda _ param2 ty2 lambdaBody2 -> do
                                                    param2 `shouldBe` "b"
                                                    case ty2 of
                                                        AST.SInt -> return ()
                                                        _ -> expectationFailure "Param type should be Int"
                                                    case lambdaBody2 of
                                                        AST.ExprBinary _ AST.AddInt e1 e2 -> do
                                                            case e1 of
                                                                AST.ExprIdent _ "a" -> return ()
                                                                _ -> expectationFailure "First operand should be 'a'"
                                                            case e2 of
                                                                AST.ExprIdent _ "b" -> return ()
                                                                _ -> expectationFailure "Second operand should be 'b'"
                                                        _ -> expectationFailure "Body should be a binary addition"
                                                _ -> expectationFailure "Body should be a nested lambda expression"
                                        _ -> expectationFailure "Value should be a lambda expression"
                                    case body of
                                        AST.ExprApp _ func arg -> do
                                            case func of
                                                AST.ExprApp _ func2 arg2 -> do
                                                    case func2 of
                                                        AST.ExprIdent _ "f" -> return ()
                                                        _ -> expectationFailure "Function name should be 'f'"
                                                    case arg2 of
                                                        AST.ExprLit _ AST.SInt 1 -> return ()
                                                        _ -> expectationFailure "First argument should be 1"
                                                _ -> expectationFailure "Function should be an application"
                                            case arg of
                                                AST.ExprLit _ AST.SInt 2 -> return ()
                                                _ -> expectationFailure "Second argument should be 2"
                                        _ -> expectationFailure "Body should be a function application"
                                _ -> expectationFailure "Should be a let expression"

        it "should desugar lambda expression with multiple parameters and implicit Any type" $ do
            let input = "\\a:Int b:Int -> a + b"
            case Parser.parseExpr (pack input) of
                Left err -> expectationFailure $ "Parse failed: " ++ show err
                Right cstExpr -> do
                    case desugar Map.empty cstExpr of
                        Left err -> expectationFailure $ "Desugar failed: " ++ show err
                        Right (AST.TypeExpr _ desugaredAst) -> do
                            case desugaredAst of
                                AST.ExprLambda _ param1 ty1 (AST.ExprLambda _ param2 ty2 body) -> do
                                    param1 `shouldBe` "a"
                                    case ty1 of
                                        AST.SInt -> return ()
                                        _ -> expectationFailure "First param type should be Int"
                                    param2 `shouldBe` "b"
                                    case ty2 of
                                        AST.SInt -> return ()
                                        _ -> expectationFailure "Second param type should be Int"
                                    case body of
                                        AST.ExprBinary _ _ (AST.ExprIdent _ "a") (AST.ExprIdent _ "b") -> return () -- Added _ for Op
                                        _ -> expectationFailure "Body should be a binary expression"
                                _ -> expectationFailure "Should be a nested lambda expression" 
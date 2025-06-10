module DesugarSpec (spec) where

import Test.Hspec
import qualified Language.Calculator.CST.Parser as Parser
import qualified Language.Calculator.AST.Types as AST
import Language.Calculator.Desugar (desugar)
import Data.Text (pack)
import qualified Data.Map as Map
import Debug.Trace (trace)

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
                        Right (AST.TypeExpr _ ast) -> do
                            -- Verify this is a let expression
                            case ast of
                                AST.ExprLet bindings body -> do
                                    -- Verify there is one binding named 'f'
                                    length bindings `shouldBe` 1
                                    let (name, value) = head bindings
                                    name `shouldBe` "f"
                                    
                                    -- Verify the value is a lambda expression
                                    case value of
                                        AST.Exists (AST.ExprLambda param ty body) -> do
                                            -- Verify parameter name and type
                                            param `shouldBe` "a"
                                            case ty of
                                                AST.Exists AST.SInt -> return ()
                                                _ -> expectationFailure "First parameter should have type Int"
                                            
                                            -- Verify the body is another lambda
                                            case body of
                                                AST.ExprLambda param2 ty2 body2 -> do
                                                    -- Verify second parameter name and type
                                                    param2 `shouldBe` "b"
                                                    case ty2 of
                                                        AST.Exists AST.SInt -> return ()
                                                        _ -> expectationFailure "Second parameter should have type Int"
                                                    
                                                    -- Verify the body is a binary addition
                                                    case body2 of
                                                        AST.ExprBinary AST.AddInt e1 e2 -> do
                                                            -- Verify first operand is 'a'
                                                            case e1 of
                                                                AST.ExprIdent "a" -> return ()
                                                                _ -> expectationFailure "First operand should be 'a'"
                                                            -- Verify second operand is 'b'
                                                            case e2 of
                                                                AST.ExprIdent "b" -> return ()
                                                                _ -> expectationFailure "Second operand should be 'b'"
                                                        _ -> expectationFailure "Body should be an addition expression"
                                                _ -> expectationFailure "Value should be a nested lambda expression"
                                        _ -> expectationFailure "Value should be a lambda expression"
                                    
                                    -- Verify the body is a function application
                                    case body of
                                        AST.ExprApp "f" args -> do
                                            -- Verify there are two arguments
                                            length args `shouldBe` 2
                                            -- Verify first argument is 1
                                            case head args of
                                                AST.Exists (AST.ExprLit AST.SInt 1) -> return ()
                                                _ -> expectationFailure "First argument should be 1"
                                            -- Verify second argument is 2
                                            case args !! 1 of
                                                AST.Exists (AST.ExprLit AST.SInt 2) -> return ()
                                                o@_ -> trace (show o) $ expectationFailure "Second argument should be 2"
                                        _ -> expectationFailure "Body should be a function application"
                                _ -> expectationFailure "Should be a let expression" 
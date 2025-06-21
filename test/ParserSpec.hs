module ParserSpec (spec) where

import Test.Hspec
import Language.Calculator.CST.Parser (parseExpr)
import Language.Calculator.CST.Types
import Data.Text (pack)
import Language.Calculator.Common.Types (SourceToken (..))

spec :: Spec
spec = do
    describe "Parser" $ do
        it "should parse complex lambda expression with type annotations" $ do
            let input = "let f = \\a:Int b:Int -> a+b in f (1,2)"
            case parseExpr (pack input) of
                Left err -> expectationFailure $ "Parse failed: " ++ show err
                Right expr -> do
                    -- Verify this is a let expression
                    case expr of
                        ExprLet _ bindings body -> do
                            -- Verify the let binding name is "f"
                            length bindings `shouldBe` 1
                            let (name, value) = head bindings
                            case name of
                                SourceToken _ "f" -> return ()
                                _ -> expectationFailure "Let binding should be named 'f'"
                            -- Verify the value is a lambda expression
                            case value of
                                ExprLambda _ params lambdaBody -> do
                                    -- Verify there are two parameters
                                    length params `shouldBe` 2
                                    -- Verify the first parameter
                                    let (param1, type1) = head params
                                    case param1 of
                                        SourceToken _ "a" -> return ()
                                        _ -> expectationFailure "First parameter should be named 'a'"
                                    case type1 of
                                        ExprIdent (SourceToken _ "Int") -> return ()
                                        _ -> expectationFailure "First parameter should have type Int"
                                    -- Verify the second parameter
                                    let (param2, type2) = params !! 1
                                    case param2 of
                                        SourceToken _ "b" -> return ()
                                        _ -> expectationFailure "Second parameter should be named 'b'"
                                    case type2 of
                                        ExprIdent (SourceToken _ "Int") -> return ()
                                        _ -> expectationFailure "Second parameter should have type Int"
                                    -- Verify the body is a binary addition
                                    case lambdaBody of
                                        ExprBinary _ op e1 e2 -> do
                                            -- Verify the operator is addition
                                            case op of
                                                SourceToken _ OpPlus -> return ()
                                                _ -> expectationFailure "Body should be an addition expression"
                                            -- Verify first operand is 'a'
                                            case e1 of
                                                ExprIdent (SourceToken _ "a") -> return ()
                                                _ -> expectationFailure "First operand should be 'a'"
                                            -- Verify second operand is 'b'
                                            case e2 of
                                                ExprIdent (SourceToken _ "b") -> return ()
                                                _ -> expectationFailure "Second operand should be 'b'"
                                        _ -> expectationFailure "Body should be a binary expression"
                                _ -> expectationFailure "Value should be a lambda expression"
                            -- Verify the body is a function application
                            case body of
                                ExprApp _ func args -> do
                                    -- Verify the function name is "f"
                                    case func of
                                        SourceToken _ "f" -> return ()
                                        _ -> expectationFailure "Function name should be 'f'"
                                    -- Verify there are two arguments
                                    length args `shouldBe` 2
                                    -- Verify first argument is 1
                                    case head args of
                                        ExprInt (SourceToken _ 1) -> return ()
                                        _ -> expectationFailure "First argument should be 1"
                                    -- Verify second argument is 2
                                    case args !! 1 of
                                        ExprInt (SourceToken _ 2) -> return ()
                                        _ -> expectationFailure "Second argument should be 2"
                                _ -> expectationFailure "Body should be a function application"
                        _ -> expectationFailure "Should be a let expression"

        it "should parse lambda expression with multiple parameters and implicit Any type" $ do
            let input = "\\a b -> a + b"
            case parseExpr (pack input) of
                Left err -> expectationFailure $ "Parse failed: " ++ show err
                Right expr -> do
                    case expr of
                        ExprLambda _ params body -> do
                            -- Verify there are two parameters
                            length params `shouldBe` 2
                            -- Verify the first parameter 'a' with implicit Any type
                            let (param1, type1) = head params
                            case param1 of
                                SourceToken _ "a" -> return ()
                                _ -> expectationFailure "First parameter should be named 'a'"
                            case type1 of
                                ExprIdent (SourceToken _ "Any") -> return ()
                                _ -> expectationFailure "First parameter should have implicit Any type"
                            -- Verify the second parameter 'b' with implicit Any type
                            let (param2, type2) = params !! 1
                            case param2 of
                                SourceToken _ "b" -> return ()
                                _ -> expectationFailure "Second parameter should be named 'b'"
                            case type2 of
                                ExprIdent (SourceToken _ "Any") -> return ()
                                _ -> expectationFailure "Second parameter should have implicit Any type"
                            -- Verify the body is a binary addition
                            case body of
                                ExprBinary _ op e1 e2 -> do
                                    -- Verify the operator is addition
                                    case op of
                                        SourceToken _ OpPlus -> return ()
                                        _ -> expectationFailure "Body should be an addition expression"
                                    -- Verify first operand is 'a'
                                    case e1 of
                                        ExprIdent (SourceToken _ "a") -> return ()
                                        _ -> expectationFailure "First operand should be 'a'"
                                    -- Verify second operand is 'b'
                                    case e2 of
                                        ExprIdent (SourceToken _ "b") -> return ()
                                        _ -> expectationFailure "Second operand should be 'b'"
                                _ -> expectationFailure "Body should be a binary expression"
                        _ -> expectationFailure "Should be a lambda expression"

        it "should parse nested lambda expression with type annotations" $ do
            let input = "\\a:Int -> \\b:Int -> a + b"
            case parseExpr (pack input) of
                Left err -> expectationFailure $ "Parse failed: " ++ show err
                Right expr -> do
                    -- Verify this is the outer lambda expression
                    case expr of
                        ExprLambda _ params1 body1 -> do
                            -- Verify there is one parameter 'a'
                            length params1 `shouldBe` 1
                            let (param1, type1) = head params1
                            case param1 of
                                SourceToken _ "a" -> return ()
                                _ -> expectationFailure "First parameter should be named 'a'"
                            case type1 of
                                ExprIdent (SourceToken _ "Int") -> return ()
                                _ -> expectationFailure "First parameter should have type Int"
                            -- Verify the body is the inner lambda expression
                            case body1 of
                                ExprLambda _ params2 body2 -> do
                                    -- Verify there is one parameter 'b'
                                    length params2 `shouldBe` 1
                                    let (param2, type2) = head params2
                                    case param2 of
                                        SourceToken _ "b" -> return ()
                                        _ -> expectationFailure "Second parameter should be named 'b'"
                                    case type2 of
                                        ExprIdent (SourceToken _ "Int") -> return ()
                                        _ -> expectationFailure "Second parameter should have type Int"
                                    -- Verify the innermost body is a binary addition
                                    case body2 of
                                        ExprBinary _ op e1 e2 -> do
                                            case op of
                                                SourceToken _ OpPlus -> return ()
                                                _ -> expectationFailure "Body should be an addition expression"
                                            case e1 of
                                                ExprIdent (SourceToken _ "a") -> return ()
                                                _ -> expectationFailure "First operand should be 'a'"
                                            case e2 of
                                                ExprIdent (SourceToken _ "b") -> return ()
                                                _ -> expectationFailure "Second operand should be 'b'"
                                        _ -> expectationFailure "Innermost body should be a binary expression"
                                _ -> expectationFailure "Body should be the inner lambda expression"
                        _ -> expectationFailure "Should be the outer lambda expression" 
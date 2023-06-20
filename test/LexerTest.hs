module LexerTest (lexerTest) where

import Language.Calculator.CST.Lexer (tokInteger)
import Test.Hspec
import Language.Calculator.CST.Utils (run)

lexerTest :: SpecWith ()
lexerTest = describe "test lexer" $ do
    it "integer" $ do
        run tokInteger "42" `shouldBe` Right 42
import Evaluator (eval)
import Lexer (lexx)
import Parser
import Test.Hspec (describe, hspec, it, shouldBe)

cal :: String -> Either String Expr
cal input = do
  t <- lexx input
  e <- parse t
  return $ eval e

main :: IO ()
main = hspec $ do
  describe "cal" $ do
    it "1 + 2 + 3 + 4 = 10" $
      cal "1 + 2 + 3 + 4" `shouldBe` Right (Figure 10)

    it "10 - 4 - 3 - 2 - 1 = 0" $
      cal "10 - 4 - 3 - 2 - 1" `shouldBe` Right (Figure 0)

    it "10 - 4 -  ( 3 - 2 ) - 1 = 4" $
      cal "10 - 4 - ( 3 - 2 ) - 1" `shouldBe` Right (Figure 4)

    it "10 - ---4 = 14" $
      cal "10 - ---4" `shouldBe` Right (Figure 14)

    it "10 - --4 = 6" $
      cal "10 - --4" `shouldBe` Right (Figure 6)

    it "10 - ( 4 - 3 - 2 - 1 ) = 12" $
      cal "10 - ( 4 - 3 - 2 - 1 )" `shouldBe` Right (Figure 12)

    it "----3 = 3" $
      cal "----3" `shouldBe` Right (Figure 3)

    it "---3 = -3" $
      cal "---3" `shouldBe` Right (Figure (-3))

    it "10 - 4 - 3 == 3" $
      cal "10 -4 - 3 == 3" `shouldBe` Right (Boolean True)

    it "10 - 4 - 3 == 0 == false" $
      cal "10 -4 - 3 == 0 == false" `shouldBe` Right (Boolean True)

    it "true && false = false" $
      cal "true && false" `shouldBe` Right (Boolean False)

    it "true || false = true" $
      cal "true || false" `shouldBe` Right (Boolean True)

    it "true && false || true = true" $
      cal "true && false || true" `shouldBe` Right (Boolean True)

    it "true && false && true = false" $
      cal "true && false && true" `shouldBe` Right (Boolean False)

    it "true && !false && true = true" $
      cal "true && !false && true" `shouldBe` Right (Boolean True)

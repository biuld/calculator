import Evaluator (eval)
import Lexer (lexx)
import Parser
import Test.Hspec (describe, hspec, it, shouldBe)

cal :: Eq a => String -> Either String (Expr a)
cal input = do
  t <- lexx input
  e <- parse t
  return $ eval e

main :: IO ()
main = hspec $ do
  describe "cal" $ do
    it "1 + 2 + 3 + 4 = 10" $
      (cal "1 + 2 + 3 + 4" :: Either String (Expr Int)) `shouldBe` Right (Figure 10)

    it "10 - 4 - 3 - 2 - 1 = 0" $
      (cal "10 - 4 - 3 - 2 - 1" :: Either String (Expr Int)) `shouldBe` Right (Figure 0)

    it "10 - 4 -  ( 3 - 2 ) - 1 = 4" $
      (cal "10 - 4 - ( 3 - 2 ) - 1" :: Either String (Expr Int)) `shouldBe` Right (Figure 4)

    it "10 - ---4 = 14" $
      (cal "10 - ---4" :: Either String (Expr Int)) `shouldBe` Right (Figure 14)

    it "10 - --4 = 6" $
      (cal "10 - --4" :: Either String (Expr Int)) `shouldBe` Right (Figure 6)

    it "10 - ( 4 - 3 - 2 - 1 ) = 12" $
      (cal "10 - ( 4 - 3 - 2 - 1 )" :: Either String (Expr Int)) `shouldBe` Right (Figure 12)

    it "----3 = 3" $
      (cal "----3" :: Either String (Expr Int)) `shouldBe` Right (Figure 3)

    it "---3 = -3" $
      (cal "---3" :: Either String (Expr Int)) `shouldBe` Right (Figure (-3))

    it "true && false = false" $
      (cal "true && false" :: Either String (Expr Bool)) `shouldBe` Right (Boolean False)
    
    it "true || false = true" $
      (cal "true || false" :: Either String (Expr Bool)) `shouldBe` Right (Boolean True)
    
    it "true && false || true = true" $
      (cal "true && false || true" :: Either String (Expr Bool)) `shouldBe` Right (Boolean True)
    
    it "true && false && true = false" $
      (cal "true && false && true" :: Either String (Expr Bool)) `shouldBe` Right (Boolean False)
        
    it "true && !false && true = true" $
      (cal "true && !false && true" :: Either String (Expr Bool)) `shouldBe` Right (Boolean True)

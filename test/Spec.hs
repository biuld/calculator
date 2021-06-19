import Common
import Utils
import Test.Hspec (describe, hspec, it, shouldBe)
import Control.Monad.Trans

main :: IO ()
main = hspec $ do
  describe "xdP" $ do
    it "1 + 2 + 3 + 4 = 10" $
      xdP "1 + 2 + 3 + 4" `shouldBe` Right (Figure 10)

    it "10 - 4 - 3 - 2 - 1 = 0" $
      xdP "10 - 4 - 3 - 2 - 1" `shouldBe` Right (Figure 0)

    it "10 - 4 -  ( 3 - 2 ) - 1 = 4" $
      xdP "10 - 4 - ( 3 - 2 ) - 1" `shouldBe` Right (Figure 4)

    it "10 - ---4 = 14" $
      xdP "10 - ---4" `shouldBe` Right (Figure 14)

    it "10 - --4 = 6" $
      xdP "10 - --4" `shouldBe` Right (Figure 6)

    it "10 - ( 4 - 3 - 2 - 1 ) = 12" $
      xdP "10 - ( 4 - 3 - 2 - 1 )" `shouldBe` Right (Figure 12)

    it "----3 = 3" $
      xdP "----3" `shouldBe` Right (Figure 3)

    it "---3 = -3" $
      xdP "---3" `shouldBe` Right (Figure (-3))

    it "10 - 4 - 3 == 3" $
      xdP "10 -4 - 3 == 3" `shouldBe` Right (Boolean True)

    it "10 - 4 - 3 == 0 == false" $
      xdP "10 -4 - 3 == 0 == false" `shouldBe` Right (Boolean True)

    it "true && false = false" $
      xdP "true && false" `shouldBe` Right (Boolean False)

    it "true || false = true" $
      xdP "true || false" `shouldBe` Right (Boolean True)

    it "true && false || true = true" $
      xdP "true && false || true" `shouldBe` Right (Boolean True)

    it "true && false && true = false" $
      xdP "true && false && true" `shouldBe` Right (Boolean False)

    it "true && !false && true = true" $
      xdP "true && !false && true" `shouldBe` Right (Boolean True)

    it "dangling else" $
      xdP "if true if false 1 else 2" `shouldBe` Right (Figure 2)

    it "(1, 2)" $
      xdP "(1, 2)" `shouldBe` Right (Group [Figure 1, Figure 2])

    it "((1-3)*3, (1, 2))" $
      xdP "((1-3)*3, (1, 2))" `shouldBe` Right (Group [Figure $ -6, Group [Figure 1, Figure 2]])

    it "(def foo(a,b) {a+b}, foo(1,2))" $
      xdP "(def foo(a,b) {a+b}, foo(1,2))" `shouldBe` Right (Group [Unit, Figure 3])

    it "(def fa(a) { if (a==1) 1 else a*fa(a-1)}, fa(6))" $
      xdP "(def fa(a) { if (a==1) 1 else a*fa(a-1)}, fa(6))" `shouldBe` Right (Group [Unit, Figure 720])
import Common
import Utils
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "xd" $ do
    it "1 + 2 + 3 + 4 = 10" $
      xd "1 + 2 + 3 + 4" `shouldBe` Right (Figure 10)

    it "10 - 4 - 3 - 2 - 1 = 0" $
      xd "10 - 4 - 3 - 2 - 1" `shouldBe` Right (Figure 0)

    it "10 - 4 -  ( 3 - 2 ) - 1 = 4" $
      xd "10 - 4 - ( 3 - 2 ) - 1" `shouldBe` Right (Figure 4)

    it "10 - ---4 = 14" $
      xd "10 - ---4" `shouldBe` Right (Figure 14)

    it "10 - --4 = 6" $
      xd "10 - --4" `shouldBe` Right (Figure 6)

    it "10 - ( 4 - 3 - 2 - 1 ) = 12" $
      xd "10 - ( 4 - 3 - 2 - 1 )" `shouldBe` Right (Figure 12)

    it "----3 = 3" $
      xd "----3" `shouldBe` Right (Figure 3)

    it "---3 = -3" $
      xd "---3" `shouldBe` Right (Figure (-3))

    it "10 - 4 - 3 == 3" $
      xd "10 -4 - 3 == 3" `shouldBe` Right (Boolean True)

    it "10 - 4 - 3 == 0 == false" $
      xd "10 -4 - 3 == 0 == false" `shouldBe` Right (Boolean True)

    it "true && false = false" $
      xd "true && false" `shouldBe` Right (Boolean False)

    it "true || false = true" $
      xd "true || false" `shouldBe` Right (Boolean True)

    it "true && false || true = true" $
      xd "true && false || true" `shouldBe` Right (Boolean True)

    it "true && false && true = false" $
      xd "true && false && true" `shouldBe` Right (Boolean False)

    it "true && !false && true = true" $
      xd "true && !false && true" `shouldBe` Right (Boolean True)

    it "dangling else" $
      xd "if true if false 1 else 2" `shouldBe` Right (Figure 2)

    it "(1, 2)" $
      xd "(1, 2)" `shouldBe` Right (Group [Figure 1, Figure 2])

    it "((1-3)*3, (1, 2))" $
      xd "((1-3)*3, (1, 2))" `shouldBe` Right (Group [Figure $ -6, Group [Figure 1, Figure 2]])

    it "(def foo(a,b) {a+b}, foo(1,2))" $
      xd "(def foo(a,b) {a+b}, foo(1,2))" `shouldBe` Right (Group [Unit, Figure 3])

    it "(def fa(a) { if (a==1) 1 else a*fa(a-1)}, fa(6))" $
      xd "(def fa(a) { if (a==1) 1 else a*fa(a-1)}, fa(6))" `shouldBe` Right (Group [Unit, Figure 720])
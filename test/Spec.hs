import Control.Monad.Except
import Control.Monad.State.Strict
import Evaluator (eval)
import Lexer (lexx)
import Optics
import Parser
import Test.Hspec (describe, hspec, it, shouldBe)

evalHelper :: String -> Either String Expr
evalHelper input = do
  t <- lexx input
  evalState (runExceptT (do parse; eval)) (emptyContext & tokens .~ t)

main :: IO ()
main = hspec $ do
  describe "evalHelper" $ do
    it "1 + 2 + 3 + 4 = 10" $
      evalHelper "1 + 2 + 3 + 4" `shouldBe` Right (Figure 10)

    it "10 - 4 - 3 - 2 - 1 = 0" $
      evalHelper "10 - 4 - 3 - 2 - 1" `shouldBe` Right (Figure 0)

    it "10 - 4 -  ( 3 - 2 ) - 1 = 4" $
      evalHelper "10 - 4 - ( 3 - 2 ) - 1" `shouldBe` Right (Figure 4)

    it "10 - ---4 = 14" $
      evalHelper "10 - ---4" `shouldBe` Right (Figure 14)

    it "10 - --4 = 6" $
      evalHelper "10 - --4" `shouldBe` Right (Figure 6)

    it "10 - ( 4 - 3 - 2 - 1 ) = 12" $
      evalHelper "10 - ( 4 - 3 - 2 - 1 )" `shouldBe` Right (Figure 12)

    it "----3 = 3" $
      evalHelper "----3" `shouldBe` Right (Figure 3)

    it "---3 = -3" $
      evalHelper "---3" `shouldBe` Right (Figure (-3))

    it "10 - 4 - 3 == 3" $
      evalHelper "10 -4 - 3 == 3" `shouldBe` Right (Boolean True)

    it "10 - 4 - 3 == 0 == false" $
      evalHelper "10 -4 - 3 == 0 == false" `shouldBe` Right (Boolean True)

    it "true && false = false" $
      evalHelper "true && false" `shouldBe` Right (Boolean False)

    it "true || false = true" $
      evalHelper "true || false" `shouldBe` Right (Boolean True)

    it "true && false || true = true" $
      evalHelper "true && false || true" `shouldBe` Right (Boolean True)

    it "true && false && true = false" $
      evalHelper "true && false && true" `shouldBe` Right (Boolean False)

    it "true && !false && true = true" $
      evalHelper "true && !false && true" `shouldBe` Right (Boolean True)

    it "dangling else" $
      evalHelper "if true if false 1 else 2" `shouldBe` Right (Figure 2)

    it "(1, 2)" $
      evalHelper "(1, 2)" `shouldBe` Right (Group [Figure 1, Figure 2])

    it "((1-3)*3, (1, 2))" $
      evalHelper "((1-3)*3, (1, 2))" `shouldBe` Right (Group [Figure $ -6, Group [Figure 1, Figure 2]])

    it "(def foo(a,b) {a+b}, foo(1,2))" $
      evalHelper "(def foo(a,b) {a+b}, foo(1,2))" `shouldBe` Right (Group [Unit, Figure 3])
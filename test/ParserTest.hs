module ParserTest (
    parserTest
) where
import Test.Hspec
import Language.Calculator.CST.Utils (run)
import Language.Calculator.CST.Types (Expr(ExprInt))
import Language.Calculator.CST.Parser (exprAtom)

parserTest :: SpecWith ()
parserTest = describe "test parser" $ do
    it "integer expr" $ do
        run exprAtom "42" `shouldBe` Right (ExprInt 42)
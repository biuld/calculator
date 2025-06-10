import Test.Hspec
import qualified ParserSpec
import qualified DesugarSpec

main :: IO ()
main = hspec $ do
  ParserSpec.spec
  DesugarSpec.spec
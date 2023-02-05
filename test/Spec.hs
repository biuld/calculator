
import Test.Hspec
import LexerTest (lexerTest)
import ParserTest (parserTest)
main :: IO ()

main = hspec $ do
    lexerTest
    parserTest
module Day03Spec (spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (pack)
import Days.Day03 (inputParser, inputParserWithContext, lineParser, lineParserWithContext, matchCorePattern, partA, partB, processLine)
import Debug.Trace
import Test.Hspec

spec :: Spec
spec = do
  describe "matchCorePattern" $ do
    it "matches a single pair correctly without context" $ do
      matchCorePattern (pack "mul(123,456)") `shouldBe` [(123, 456)]

    it "matches multiple pairs on a single line" $ do
      matchCorePattern (pack "mul(123,456) mul(789,101)") `shouldBe` [(123, 456), (789, 101)]

    it "handles empty input" $ do
      matchCorePattern (pack "") `shouldBe` []

    it "ignores invalid matches" $ do
      matchCorePattern (pack "mul(123,abc) mul(456,789)") `shouldBe` [(456, 789)]

  describe "processLine" $ do
    it "matches a single pair correctly with context" $ do
      processLine (pack "do()mul(123,456)don't()") "do()" "don't()" `shouldBe` ["do()mul(123,456)"]
      processLine (pack "mul(123,456)don't()") "do()" "don't()" `shouldBe` ["mul(123,456)"]
      processLine (pack "do()mul(123,456)") "do()" "don't()" `shouldBe` ["do()mul(123,456)"]

    it "should still match if the context is missing" $ do
      processLine (pack "mul(123,456)") "do()" "don't()" `shouldBe` ["mul(123,456)"]

    it "matches multiple pairs on a single line with context" $ do
      processLine (pack "do()mul(123,456) mul(789,101) no_mul(111,222)don't()\n") "do()" "don't()" `shouldBe` ["do()mul(123,456) mul(789,101) no_mul(111,222)"]
      processLine (pack "do()mul(456, 567) mul(789,012)") "do()" "don't()" `shouldBe` ["do()mul(456, 567) mul(789,012)"]
      processLine (pack "mul(789,012) no_mul(012,345)don't()") "do()" "don't()" `shouldBe` ["mul(789,012) no_mul(012,345)"]
      processLine (pack "don't()mul(789,012) do()mul(12, 34) abcd no_mul(012,345)don't()") "do()" "don't()" `shouldBe` ["", "mul(12, 34) abcd no_mul(012,345)"]

    it "matches multiple spans of text on a single line with context" $ do
      processLine (pack "don't()mul(789,012) do()mul(12, 34)abcddon't()no_mul(210,345)do()acmul(012,345)don't()") "do()" "don't()" `shouldBe` ["", "mul(12, 34)abcd", "acmul(012,345)"]
      processLine (pack "do()don't()no_mul(210,345)do()acmul(012,345)!mul(456,789)don't()") "do()" "don't()" `shouldBe` ["do()", "acmul(012,345)!mul(456,789)"]

  describe "inputParserWithContext" $ do
    it "parses multiple lines with valid matches" $ do
      let input = pack "do()mul(123,456)don't()\ndon't()mul(789,101) do()mul(112,131)\n"
      parseOnly inputParserWithContext input `shouldBe` Right [(123, 456), (112, 131)]

    it "handles empty input" $ do
      let input = pack ""
      parseOnly inputParserWithContext input `shouldBe` Right []

    it "parses some valid lines and ignores others" $ do
      let input = pack "do()mul(123,456)\ndo()has invalid value\nmul(789,101)\ndo()has invalid mull(113,134)andvalidmul(123,321)"
      parseOnly inputParserWithContext input `shouldBe` Right [(123, 456), (789, 101), (123, 321)]

  describe "Example Part A" $ do
    it "works correctly" $ do
      let rawInput = "do()xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,7))don't()"

      let parsedInput = parseOnly inputParser (pack rawInput)

      case parsedInput of
        Right input -> partA input `shouldBe` 177
        Left err -> expectationFailure $ "Parsing failed: " ++ err

  describe "Example Part B" $ do
    it "works correctly" $ do
      let rawInput = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,7))"

      let parsedInput = parseOnly inputParserWithContext (pack rawInput)

      case parsedInput of
        Right input -> partB input `shouldBe` 64
        Left err -> expectationFailure $ "Parsing failed: " ++ err

    it "works correctly on complex pattern" $ do
      -- notice that before the first `don't()`, we create mul that is almost complete which we complete with the next `do()`
      let rawInput = "mul(1,1)mul(1,10don't()abcdmul(1,100)do())mul(10,100)"

      let parsedInput = parseOnly inputParserWithContext (pack rawInput)

      case parsedInput of
        Right input -> partB input `shouldBe` 1001
        Left err -> expectationFailure $ "Parsing failed: " ++ err
 where
  isLeft (Left _) = True
  isLeft _ = False

module Day01Spec (spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (pack)
import Days.Day01 (inputParser, lineParser, partA, partB)
import Test.Hspec

spec :: Spec
spec = do
  describe "lineParser" $ do
    it "parses a line with two integers separated by a space" $ do
      let input = pack "123 456\n"
      parseOnly lineParser input `shouldBe` Right (123, 456)

    it "parses a line with two integers separated by multiple spaces" $ do
      let input = pack "123   456\n"
      parseOnly lineParser input `shouldBe` Right (123, 456)

    it "parses a line with two integers at the end of input without a newline" $ do
      let input = pack "123 456"
      parseOnly lineParser input `shouldBe` Right (123, 456)

    it "fails to parse a line with only one integer" $ do
      let input = pack "123\n"
      parseOnly lineParser input `shouldSatisfy` isLeft

    it "fails to parse a line with non-integer values" $ do
      let input = pack "abc 456\n"
      parseOnly lineParser input `shouldSatisfy` isLeft

    it "fails to parse a completely empty input" $ do
      let input = pack ""
      parseOnly lineParser input `shouldSatisfy` isLeft

  describe "inputParser" $ do
    it "parses input text correctly" $ do
      let rawInput =
            "3   4\n\
            \4   3\n\
            \2   5\n\
            \1   3\n\
            \3   9\n\
            \3   3"

      parseOnly inputParser rawInput `shouldBe` Right ([3, 4, 2, 1, 3, 3], [4, 3, 5, 3, 9, 3])

  describe "Example Part A" $ do
    it "works correctly" $ do
      let rawInput =
            "3   4\n\
            \4   3\n\
            \2   5\n\
            \1   3\n\
            \3   9\n\
            \3   3"

      let parsedInput = parseOnly inputParser (pack rawInput)

      case parsedInput of
        Right input -> partA input `shouldBe` 11
        Left err -> expectationFailure $ "Parsing failed: " ++ err

  describe "Example Part B" $ do
    it "works correctly" $ do
      let rawInput =
            "3   4\n\
            \4   3\n\
            \2   5\n\
            \1   3\n\
            \3   9\n\
            \3   3"

      let parsedInput = parseOnly inputParser (pack rawInput)

      case parsedInput of
        Right input -> partB input `shouldBe` 31
        Left err -> expectationFailure $ "Parsing failed: " ++ err
 where
  isLeft (Left _) = True
  isLeft _ = False

module Day02Spec (spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text (pack)
import Days.Day02 (inputParser, lineParser, partA, partB)
import Test.Hspec

spec :: Spec
spec = do
  describe "lineParser" $ do
    it "parses a line with two integers separated by a space" $ do
      let input = pack "123 456\n"
      parseOnly lineParser input `shouldBe` Right [123, 456]

    it "parses a line with two integers separated by multiple spaces" $ do
      let input = pack "123   456\n"
      parseOnly lineParser input `shouldBe` Right [123, 456]

    it "parses a line with two integers at the end of input without a newline" $ do
      let input = pack "123 456"
      parseOnly lineParser input `shouldBe` Right [123, 456]

    it "parses a line with only one integer" $ do
      let input = pack "123\n"
      parseOnly lineParser input `shouldBe` Right [123]

    it "fails to parse a completely empty input" $ do
      let input = pack ""
      parseOnly lineParser input `shouldSatisfy` isLeft

    it "fails to parse a line with non-integer values" $ do
      let input = pack "abc 456\n"
      parseOnly lineParser input `shouldSatisfy` isLeft

  describe "inputParser" $ do
    it "parses input text correctly" $ do
      let rawInput =
            "7 6 4 2 1\n\
            \1 2 7 8 9\n\
            \9 7 6 2 1\n\
            \1 3 2 4 5\n\
            \8 6 4 4 1\n\
            \1 3 6 7 9"

      parseOnly inputParser rawInput `shouldBe` Right [[7, 6, 4, 2, 1], [1, 2, 7, 8, 9], [9, 7, 6, 2, 1], [1, 3, 2, 4, 5], [8, 6, 4, 4, 1], [1, 3, 6, 7, 9]]

    it "parses input text correctly" $ do
      let rawInput =
            "7 6 4 2 1\n\n\
            \9 7 6 2 1"

      parseOnly inputParser rawInput `shouldBe` Right [[7, 6, 4, 2, 1], [], [9, 7, 6, 2, 1]]

  describe "Example Part A" $ do
    it "works correctly" $ do
      let rawInput =
            "7 6 4 2 1\n\
            \1 2 7 8 9\n\
            \9 7 6 2 1\n\
            \1 3 2 4 5\n\
            \8 6 4 4 1\n\
            \1 3 6 7 9"

      let parsedInput = parseOnly inputParser (pack rawInput)

      case parsedInput of
        Right input -> partA input `shouldBe` 2
        Left err -> expectationFailure $ "Parsing failed: " ++ err

  describe "Example Part B" $ do
    it "works correctly" $ do
      let rawInput =
            "7 6 4 2 1\n\
            \1 2 7 8 9\n\
            \9 7 6 2 1\n\
            \1 3 2 4 5\n\
            \8 6 4 4 1\n\
            \1 3 6 7 9"

      let parsedInput = parseOnly inputParser (pack rawInput)

      case parsedInput of
        Right input -> partB input `shouldBe` 4
        Left err -> expectationFailure $ "Parsing failed: " ++ err
 where
  isLeft (Left _) = True
  isLeft _ = False

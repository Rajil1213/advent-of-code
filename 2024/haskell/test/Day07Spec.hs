module Day07Spec (spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text qualified as T
import Days.Day07 (Operation (..), dfs, getOperator, inputParser, lineParser, partA, partB)
import Debug.Trace
import Test.Hspec

data TestCase = TestCase {target :: Int, values :: [Int], expected :: Bool}

spec :: Spec
spec = do
  describe "inputParser" $ do
    it "works" $ do
      let input =
            "190: 10 19\n\
            \3267: 81 40 27\n\
            \83: 17 5"

      parseOnly inputParser input `shouldBe` Right [(190, [10, 19]), (3267, [81, 40, 27]), (83, [17, 5])]

  describe "dfs" $ do
    describe "works without concatentation" $ do
      let testCases =
            [ TestCase 190 [10, 19] True
            , TestCase 3267 [81, 40, 27] True
            , TestCase 83 [17, 5] False
            , TestCase 21037 [9, 7, 18, 13] False
            , TestCase 292 [11, 6, 16, 20] True
            ] ::
              [TestCase]

      mapM_
        ( \(label, TestCase target values expected) ->
            it ("case: " ++ show label) $ dfs target [Add, Multiply] values 0 `shouldBe` expected
        )
        $ zip [1 ..] testCases

    describe "works with concatentation" $ do
      let testCases =
            [ TestCase 156 [15, 6] True
            , TestCase 1951 [4, 33, 4, 7, 120, 7, 4, 3, 1, 8, 2, 8] True
            , TestCase 7290 [6, 8, 6, 15, 1] True
            , TestCase 7290 [7, 8, 9, 18, 2] False
            ] ::
              [TestCase]

      mapM_
        ( \(label, TestCase target values expected) ->
            it ("case: " ++ show label) $ dfs target [Add, Multiply, Concatenate] values 0 `shouldBe` expected
        )
        $ zip [1 ..] testCases

  describe "partA" $ do
    it "works" $ do
      let input =
            "190: 10 19\n\
            \3267: 81 40 27\n\
            \83: 17 5\n\
            \156: 15 6\n\
            \7290: 6 8 6 15\n\
            \161011: 16 10 13\n\
            \192: 17 8 14\n\
            \21037: 9 7 18 13\n\
            \292: 11 6 16 20"

      case parseOnly inputParser input of
        Right entries -> partA entries `shouldBe` 3749
        Left _ -> expectationFailure "failed to parse input"

  describe "partB" $ do
    it "works" $ do
      let input =
            "190: 10 19\n\
            \3267: 81 40 27\n\
            \83: 17 5\n\
            \156: 15 6\n\
            \7290: 6 8 6 15\n\
            \161011: 16 10 13\n\
            \192: 17 8 14\n\
            \21037: 9 7 18 13\n\
            \292: 11 6 16 20"

      case parseOnly inputParser input of
        Right entries -> partB entries `shouldBe` 11387
        Left _ -> expectationFailure "failed to parse input"
 where
  isLeft (Left _) = True
  isLeft _ = False

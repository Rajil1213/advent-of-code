module Day04Spec (spec) where

import Data.Attoparsec.Text (parseOnly)
import Data.Text qualified as T
import Days.Day04 (countHorizontal, countVertical, getLeftDiagonalIndices, getRightDiagonalIndices, inputParser, lineParser, partA, partB)
import Debug.Trace
import Test.Hspec

spec :: Spec
spec = do
  describe "inputParser" $ do
    it "should parse the input correctly" $ do
      let rawInput =
            "..X...\n\
            \.SAMX.\n\
            \.A..A.\n\
            \XMAS.S\n\
            \.X...."

      parseOnly inputParser (T.pack rawInput) `shouldBe` Right ["..X...", ".SAMX.", ".A..A.", "XMAS.S", ".X...."]

  describe "countHorizontal" $ do
    it "should count the horizontal matches correctly" $ do
      let line = T.pack "abc..abcabc.abc"
      let textToSearch = T.pack "abc"

      countHorizontal line textToSearch 0 `shouldBe` 4

    it "should count the overlapping horizontal matches correctly" $ do
      let line = T.pack "abca..abcabca.abca"
      let textToSearch = T.pack "abca"

      countHorizontal line textToSearch 0 `shouldBe` 4

  describe "countVertical" $ do
    it "should count the single forward vertical match correctly" $ do
      let grid = map T.pack ["..X...", ".SMMX.", "..A.A.", "XMSA.S", ".X...."]

      countVertical grid (T.pack "XMAS") 0 `shouldBe` 1
      countVertical grid (T.pack "XMAS") 2 `shouldBe` 0

    it "should count the single reverse vertical match correctly" $ do
      let grid = map T.pack ["..X...", ".SAMX.", ".A..A.", "XMAS.S", ".X...."]

      countVertical grid (T.pack "XMAS") 1 `shouldBe` 1

    it "should count the multiple vertical matches correctly" $ do
      let grid = map T.pack ["..X...", ".SAMX.", ".A..M.", "XMASAS", ".X..S."]

      countVertical grid (T.pack "XMAS") 1 `shouldBe` 2

  describe "getRightDiagonalIndices" $ do
    it "should count the single forward right diagonal match correctly" $ do
      let rawInput =
            "..X...\n\
            \.SAMX.\n\
            \.A..A.\n\
            \XMAS.S\n\
            \.X...."

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right grid -> getRightDiagonalIndices grid (T.pack "XMAS") 0 `shouldBe` [2]
        Left err -> expectationFailure $ "Parsing failed: " ++ err

    it "should count the single reverse right diagonal match correctly" $ do
      let rawInput =
            "..S...\n\
            \.SAAX.\n\
            \.A..M.\n\
            \XMAS.X\n\
            \.X...."

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right grid -> getRightDiagonalIndices grid (T.pack "XMAS") 0 `shouldBe` [2]
        Left err -> expectationFailure $ "Parsing failed: " ++ err

    it "should count multiple right diagonal matches correctly" $ do
      let rawInput =
            "..S...\n\
            \.SXAX.\n\
            \.AAMM.\n\
            \XMAMAX\n\
            \.X..XS"

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right grid -> getRightDiagonalIndices grid (T.pack "XMAS") 1 `shouldBe` [1, 2]
        Left err -> expectationFailure $ "Parsing failed: " ++ err

  describe "getLeftDiagonalIndices" $ do
    it "should count the single forward left diagonal match correctly" $ do
      let rawInput =
            "...X..\n\
            \.SMMX.\n\
            \.A..A.\n\
            \SMAS.S\n\
            \.X...."

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right grid -> getLeftDiagonalIndices grid (T.pack "XMAS") 0 `shouldBe` [3]
        Left err -> expectationFailure $ "Parsing failed: " ++ err

    it "should count the single reverse left diagonal match correctly" $ do
      let rawInput =
            "..S.S.\n\
            \.SAAX.\n\
            \.AM.M.\n\
            \XXAS.X\n\
            \.X...."

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right grid -> getLeftDiagonalIndices grid (T.pack "XMAS") 0 `shouldBe` [4]
        Left err -> expectationFailure $ "Parsing failed: " ++ err

    it "should count multiple left diagonal matches correctly" $ do
      let rawInput =
            "...S.X\n\
            \.SAS.X\n\
            \.MAMM.\n\
            \XMAAAX\n\
            \XXS.XS"

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right grid -> getLeftDiagonalIndices grid (T.pack "XMAS") 1 `shouldBe` [3, 5]
        Left err -> expectationFailure $ "Parsing failed: " ++ err

  describe "Example Part A" $ do
    it "works correctly" $ do
      let rawInput =
            "MMMSXXMASM\n\
            \MSAMXMSMSA\n\
            \AMXSXMAAMM\n\
            \MSAMASMSMX\n\
            \XMASAMXAMM\n\
            \XXAMMXXAMA\n\
            \SMSMSASXSS\n\
            \SAXAMASAAA\n\
            \MAMMMXMMMM\n\
            \MXMXAXMASX"

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right input -> partA input `shouldBe` 18
        Left err -> expectationFailure $ "Parsing failed: " ++ err

  describe "Example Part B" $ do
    it "works correctly" $ do
      let rawInput =
            "MMMSXXMASM\n\
            \MSAMXMSMSA\n\
            \AMXSXMAAMM\n\
            \MSAMASMSMX\n\
            \XMASAMXAMM\n\
            \XXAMMXXAMA\n\
            \SMSMSASXSS\n\
            \SAXAMASAAA\n\
            \MAMMMXMMMM\n\
            \MXMXAXMASX"

      let parsedInput = parseOnly inputParser (T.pack rawInput)

      case parsedInput of
        Right input -> partB input `shouldBe` 9
        Left err -> expectationFailure $ "Parsing failed: " ++ err
 where
  isLeft (Left _) = True
  isLeft _ = False

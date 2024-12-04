module Days.Day04 (runDay, partA, partB, inputParser, lineParser, countHorizontal, countVertical, getLeftDiagonalIndices, getRightDiagonalIndices) where

import Data.List (findIndices, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, endOfLine, sepBy, takeTill)
import Data.Ix (Ix (inRange))
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = lineParser `sepBy` endOfLine

lineParser :: Parser T.Text
lineParser = takeTill (== '\n')

------------ TYPES ------------
type Input = Grid
type Grid = [T.Text]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA grid =
  sum $
    [ countHorizontal (grid !! lineNum) textToSearch 0
      + countVertical grid textToSearch lineNum
      + length (getRightDiagonalIndices grid textToSearch lineNum)
      + length (getLeftDiagonalIndices grid textToSearch lineNum)
    | lineNum <- take (length grid) [0 ..]
    ]
 where
  textToSearch = T.pack "XMAS"

isValid :: T.Text -> T.Text -> Bool
isValid s textToSearch = textToSearch `T.isPrefixOf` s || T.reverse textToSearch `T.isPrefixOf` s

countHorizontal :: T.Text -> T.Text -> Int -> Int
countHorizontal line textToSearch count
  | T.null line = count
  | line `isValid` textToSearch = countHorizontal (T.tail line) textToSearch (count + 1)
  | otherwise = countHorizontal (T.tail line) textToSearch count

countVertical :: Grid -> T.Text -> Int -> Int
countVertical grid textToSearch row =
  let rowSize = T.length $ grid !! row
   in length $ filter matches [vertical row col | col <- take rowSize [0 ..]]
 where
  matches :: T.Text -> Bool
  matches s = s == textToSearch || s == T.reverse textToSearch

  vertical :: Int -> Int -> T.Text
  vertical row col =
    let remainingRows = drop row grid
        needleLength = T.length textToSearch
     in T.pack $ [T.index line col | line <- take needleLength remainingRows, col < T.length line]

------------ PART B ------------
partB :: Input -> OutputB
partB grid =
  let textToSearch = T.pack "MAS"
   in sum $
        [ length $
          -- NOTE: the `rightIndex` lies on the left of the `leftIndex` as the right diagonal starts earlier than the left
          filter id [(leftIndex - (T.length textToSearch - 1)) `elem` rightIndices | leftIndex <- leftIndices]
        | lineNum <- take (length grid) [0 ..]
        , let leftIndices = getLeftDiagonalIndices grid textToSearch lineNum
        , let rightIndices = getRightDiagonalIndices grid textToSearch lineNum
        ]

------------ COMMON ------------

getRightDiagonalIndices :: Grid -> T.Text -> Int -> [Int]
getRightDiagonalIndices grid textToSearch row =
  let rowSize = T.length $ grid !! row
   in findIndices matches [diagonal row col | col <- take rowSize [0 ..]]
 where
  matches :: T.Text -> Bool
  matches s = s == textToSearch || s == T.reverse textToSearch

  diagonal :: Int -> Int -> T.Text
  diagonal row col =
    let remainingRows = take (length (drop row grid)) [0 ..]
        needleLength = T.length textToSearch
     in T.pack $ [T.index (grid !! (row + lineNum)) (col + lineNum) | lineNum <- take needleLength remainingRows, (col + lineNum) < T.length (grid !! (row + lineNum))]

getLeftDiagonalIndices :: Grid -> T.Text -> Int -> [Int]
getLeftDiagonalIndices grid textToSearch row =
  let rowSize = T.length $ grid !! row
   in findIndices matches [diagonal row col | col <- take rowSize [0 ..]]
 where
  matches :: T.Text -> Bool
  matches s = s == textToSearch || s == T.reverse textToSearch

  diagonal :: Int -> Int -> T.Text
  diagonal row col =
    let remainingRows = take (length (drop row grid)) [0 ..]
        needleLength = T.length textToSearch
     in T.pack $ [T.index (grid !! (row + lineNum)) (col - lineNum) | lineNum <- take needleLength remainingRows, inRange (0, T.length (grid !! (row + lineNum)) - 1) (col - lineNum)]

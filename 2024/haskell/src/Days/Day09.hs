module Days.Day09 (runDay, partA, partB, inputParser, lineParser) where

import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Void
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = do
  pairs <- many' lineParser
  return (map fst pairs, map snd pairs)

lineParser :: Parser (Int, Int)
lineParser = do
  a <- decimal
  skipSpace
  b <- decimal
  endOfLine <|> endOfInput
  return (a, b)

------------ TYPES ------------
type Input = ([Int], [Int])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA pair = sum . map abs $ zipWith (-) (sort firstList) (sort secondList)
 where
  firstList = fst pair
  secondList = snd pair

------------ PART B ------------
partB :: Input -> OutputB
partB pair = sum score
 where
  score = [x * x `countElem` snd pair | x <- fst pair]

countElem :: Int -> [Int] -> Int
countElem x lst = length [a | a <- lst, x == a]

module Days.Day02 (runDay, partA, partB, inputParser, lineParser) where

import Debug.Trace (trace)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec

import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, atEnd, decimal, endOfInput, endOfLine, many', takeTill)
import Data.List (elemIndex)
import Data.Text (unpack)
import Data.Void
import Program.RunDay qualified as R (Day, runDay)
import Text.ParserCombinators.ReadP (skipSpaces)

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many' lineParser

lineParser :: Parser [Int]
lineParser = do
  atEnd <- atEnd
  if atEnd
    then fail "no more input"
    else do
      line <- takeTill (== '\n')
      _ <- endOfLine <|> endOfInput
      case traverse safeRead (words $ unpack line) of
        Just nums -> return nums
        Nothing -> fail "non-integer value in line"
 where
  safeRead :: String -> Maybe Int
  safeRead s = case reads s of
    [(n, "")] -> Just n
    _ -> Nothing

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA lst = length $ filter safeWithoutRemoval lst

------------ PART B ------------
partB :: Input -> OutputB
partB lst = length $ filter safeWithRemoval lst

------------ Helpers ------------

safeWithoutRemoval :: [Int] -> Bool
safeWithoutRemoval lst =
  let diffs = computeDiffs lst
   in all isValidIncreasing diffs || all isValidDecreasing diffs

computeDiffs :: [Int] -> [Int]
-- shift the list by one and diff with unshifted to get succcessive diffs
computeDiffs singleLst = zipWith (-) (tail singleLst) singleLst

isValidIncreasing :: Int -> Bool
isValidIncreasing diff =
  diff >= 1 && diff <= 3

isValidDecreasing :: Int -> Bool
isValidDecreasing diff =
  diff <= -1 && diff >= -3

safeWithRemoval :: [Int] -> Bool
safeWithRemoval singleList =
  -- trace ("safeWithoutRemoval: " ++ show (safeWithoutRemoval singleList) ++ " and oneOfSubListsIsSafe: " ++ show (oneOfSubListsIsSafe singleList) ++ " for: " ++ show singleList) $
  safeWithoutRemoval singleList || oneOfSubListsIsSafe singleList
 where
  oneOfSubListsIsSafe :: [Int] -> Bool
  oneOfSubListsIsSafe singleList =
    or
      [ -- trace ("safeWithoutRemoval: " ++ show (safeWithoutRemoval subList) ++ " for: " ++ show subList) $
      safeWithoutRemoval subList
      | subList <- computeSubLists singleList
      ]

computeSubLists :: [Int] -> [[Int]]
computeSubLists singleList = [[singleList !! indexToRetain | indexToRetain <- rangeOverLength, indexToRetain /= indexToRemove] | indexToRemove <- rangeOverLength]
 where
  rangeOverLength = take (length singleList) [0 ..]

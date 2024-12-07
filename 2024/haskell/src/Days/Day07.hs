module Days.Day07 (runDay, partA, partB, inputParser, lineParser, dfs, getOperator, Operation (Add, Multiply, Concatenate)) where

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
inputParser = many' lineParser

lineParser :: Parser (Int, [Int])
lineParser = do
  testVal <- decimal
  char ':'
  skipSpace
  values <- decimal `sepBy` char ' '
  endOfLine <|> endOfInput
  return (testVal, values)

------------ TYPES ------------
type Input = [(Int, [Int])]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA lines = sum $ map fst $ filter (\line -> dfs (fst line) [Add, Multiply] (snd line) 0) lines

------------ PART B ------------
partB :: Input -> OutputB
partB lines = sum $ map fst $ filter (\line -> dfs (fst line) [Add, Multiply, Concatenate] (snd line) 0) lines

------------ COMMON ------------
data Operation = Add | Multiply | Concatenate

getOperator :: Operation -> (Int -> Int -> Int)
getOperator op = case op of
  Add -> (+)
  Multiply -> (*)
  Concatenate -> \a b -> read (show a ++ show b)

dfs :: Int -> [Operation] -> [Int] -> Int -> Bool
dfs target operations [] curTotal = False
dfs target operations [value] curTotal =
  or
    [ newTotal == target
    | operation <- operations
    , let op = getOperator operation
    , let newTotal = curTotal `op` value
    ]
dfs target operations (value : rest) curTotal =
  or
    [ newTotal <= target && dfs target operations rest newTotal
    | operation <- operations
    , let op = getOperator operation
    , let newTotal = curTotal `op` value
    ]

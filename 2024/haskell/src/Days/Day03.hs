module Days.Day03 (runDay, partA, partB, inputParser, inputParserWithContext, lineParser, lineParserWithContext, processLine, matchCorePattern) where

import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Text.Regex.TDFA (AllTextMatches (getAllTextMatches), (=~))

import Control.Applicative ((<|>))
import Data.Attoparsec.Text
import Data.Void
import Debug.Trace (trace)
import Program.RunDay qualified as R (Day, runDay)

runDay :: R.Day
-- runDay = R.runDay inputParser partA partB

runDay = R.runDay inputParserWithContext partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = concat <$> many' lineParser

inputParserWithContext :: Parser Input
inputParserWithContext = concat <$> many' lineParserWithContext

lineParser :: Parser [(Int, Int)]
lineParser = do
  atEnd <- atEnd
  if atEnd
    then fail "no more input"
    else do
      line <- takeTill (== '\n')
      _ <- endOfLine <|> endOfInput
      return $
        concatMap matchCorePattern $
          processLineWithoutContext line
 where
  processLineWithoutContext :: T.Text -> [T.Text]
  processLineWithoutContext line = processLine (T.pack (prefix ++ T.unpack line ++ suffix)) (T.pack prefix) (T.pack suffix)
   where
    -- add dummy prefix, suffix to the end of the line to reuse `processLine`
    prefix = "START" :: String
    suffix = "STOP" :: String

lineParserWithContext :: Parser [(Int, Int)]
lineParserWithContext = do
  atEnd <- atEnd
  if atEnd
    then fail "no more input"
    else do
      wholeText <- takeText <* endOfInput
      return $
        -- trace ("Processed line is: " ++ show (processLineWithoutContext wholeText) ++ "\n") $
        processLineWithoutContext wholeText
 where
  processLineWithoutContext :: T.Text -> [(Int, Int)]
  processLineWithoutContext line =
    concatMap matchCorePattern $
      -- trace ("raw chunks is: " ++ show (processLine line (T.pack prefix) (T.pack suffix)) ++ "\n") $
      processLine line (T.pack prefix) (T.pack suffix)
   where
    prefix = "do()" :: String
    suffix = "don't()" :: String

processLine :: T.Text -> T.Text -> T.Text -> [T.Text]
processLine line prefix suffix = process line "" True []
 where
  process :: T.Text -> T.Text -> Bool -> [T.Text] -> [T.Text]
  process remaining currentSpan entered chunks
    | T.null remaining && T.null currentSpan = chunks -- Base case: nothing in current span and nothing remaining
    | T.null remaining = chunks ++ [currentSpan] -- Base case: no more characters left to process but current span is non-empty
    | entered -- inside a valid context
      =
        -- trace ("entered: " ++ show remaining) $
        if suffix `T.isPrefixOf` remaining -- encountered the start of suffix in the remaining
          then
            -- trace ("found suffix: " ++ show remaining ++ " at chunks: " ++ show chunks ++ "\n") $
            process (T.drop (T.length suffix) remaining) (T.pack "") False (chunks ++ [currentSpan]) -- skip over the suffix and reset the current span
          else process (T.tail remaining) (currentSpan `T.snoc` T.head remaining) entered chunks -- add the character to list of valid chars
    | otherwise =
        -- trace ("found prefix: " ++ show remaining ++ " at chunks: " ++ show chunks) $
        if prefix `T.isPrefixOf` remaining -- encountered the start of prefix in the remaining
          then
            -- trace "found prefix" $
            process (T.drop (T.length prefix) remaining) (T.pack "") True chunks -- skip over the prefix and start a new chunk
          else process (T.tail remaining) currentSpan entered chunks -- skip the character

matchCorePattern :: T.Text -> [(Int, Int)]
matchCorePattern validSpan =
  let matches = getAllTextMatches (T.unpack validSpan =~ corePattern :: AllTextMatches [] String)
   in mapMaybe parseMatch matches

corePattern :: String
corePattern = "mul\\(([0-9]{1,3}),([0-9]{1,3})\\)+"

parseMatch :: String -> Maybe (Int, Int)
parseMatch match =
  let pattern = "([0-9]{1,3}),([0-9]{1,3})" :: String
      (_, _, _, [a, b]) = match =~ pattern :: (String, String, String, [String])
   in case (readMaybe a, readMaybe b) of
        (Just x, Just y) -> Just (x, y)
        _ -> Nothing

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA pairs = sum $ map (uncurry (*)) pairs

------------ PART B ------------
partB :: Input -> OutputB
partB pairs = sum $ map (uncurry (*)) pairs

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day04 where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, groupBy, intersect, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 13, Problem solveA "Problem", Case solveB "Test" 30, Problem solveB "Problem"]

data Card = Card {winning :: [Int], mine :: [Int]}

solveA :: [String] -> Int
solveA = sum . map (calculateScore . countMatches . readCard . T.pack)

readCard :: T.Text -> Card
readCard line =
  let [gameIdString, winningString, mineString] = map T.strip $ T.split (`elem` [':', '|']) line
      winning = readNumbers winningString
      mine = readNumbers mineString
   in Card {..}
  where
    readNumbers = map (read . T.unpack) . filter (not . T.null) . T.splitOn " "

countMatches :: Card -> Int
countMatches (Card winning mine) = length $ intersect winning mine

calculateScore :: Int -> Int
calculateScore matchCount = if matchCount == 0 then 0 else 2 ^ (matchCount -1)

solveB :: [String] -> Int
solveB lines = 0
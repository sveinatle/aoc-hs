{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day05 where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, find, groupBy, intersect, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 35, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

data Range = Range {destStart :: Int, srcStart :: Int, count :: Int} deriving (Show)

newtype Mapping = Mapping [Range] deriving (Show)

solveA :: [String] -> Int
solveA = minimum . mapSeedsToLocation . readProblem
  where
    mapSeedsToLocation :: ([Int], [Mapping]) -> [Int]
    mapSeedsToLocation (seeds, mappings) = map (mapSeedToLocation mappings) seeds

    mapSeedToLocation :: [Mapping] -> Int -> Int
    mapSeedToLocation mappings seed = foldl applyMapping seed mappings

    applyMapping seed (Mapping ranges) = case find (isSeedInRange seed) ranges of
      Just Range {..} -> destStart + seed - srcStart
      Nothing -> seed

    isSeedInRange :: Int -> Range -> Bool
    isSeedInRange seed Range {..} = seed >= srcStart && seed - srcStart < count

readProblem :: [String] -> ([Int], [Mapping])
readProblem lines =
  let seeds = map read . tail . words . head $ lines
      mappings = map readMapping . splitOn [""] . filter (notElem ':') . drop 2 $ lines
   in (seeds, mappings)
  where
    readMapping :: [String] -> Mapping
    readMapping lines = Mapping $ map readRange lines

    readRange :: String -> Range
    readRange line =
      case map read $ words line of
        [destStart, srcStart, count] -> Range {..}
        _ -> error $ "Expected 3 words. Got: " ++ line

solveB :: [String] -> Int
solveB lines = 0
module Day01 where

import Data.List
import DayProblem
import Debug.Trace

log2 v = trace (show v) v

cases = [Case solveA "Test" 24000, Problem solveA "Problem", Case solveB "Test" 45000, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines = maximum $ sumGroups lines

sumGroups lines =
  let lineGroups = groupBy (\a b -> (not . null) b) lines
   in map sumGroup lineGroups

sumGroup :: [String] -> Int
sumGroup lines = sum $ map read $ filter (not . null) lines

solveB :: [String] -> Int
solveB lines =
  let elvesSorted = sort $ sumGroups lines
      top3 = drop (length elvesSorted - 3) elvesSorted
   in sum top3

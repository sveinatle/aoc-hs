module Day01 where

import Data.List
import DayProblem
import Debug.Trace

log2 v = trace (show v) v

cases = [Case solveA "Test" 24000, Problem solveA "Problem", Case solveB "Test" 45000, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines = maximum $ sumGroups lines

groupLines :: [String] -> [[String]]
groupLines = groupBy (\a b -> (not . null) b)

sumGroups :: [String] -> [Int]
sumGroups = map sumGroup . groupLines

sumGroup :: [String] -> Int
sumGroup = sum . map read . filter (not . null)

solveB :: [String] -> Int
solveB lines =
  let top3 l = (drop (length l - 3) . sort) l
   in (sum . top3 . sumGroups) lines

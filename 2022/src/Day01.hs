module Day01 where

import DayProblem

cases = [Case solveA "Test" 7, Problem solveA "Problem", Case solveB "Test" 5, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines = countInc 0 (map read lines)

countInc :: Int -> [Int] -> Int
countInc sum [] = sum
countInc sum [a] = sum
countInc sum (a : b : rest)
  | a < b = countInc (sum + 1) (b : rest)
  | otherwise = countInc sum (b : rest)

solveA2 :: [String] -> Int
solveA2 lines = countIfInc $map read lines

countIfInc :: [Int] -> Int
countIfInc nums =
  let pairs = zip nums (tail nums)
   in length $ filter (uncurry (<)) pairs

solveB :: [String] -> Int
solveB lines = countIfInc $sumBy3 $map read lines

sumBy3 :: [Int] -> [Int]
sumBy3 nums =
  let threes = zip3 nums (tail nums) (tail $tail nums)
      sum3 (x, y, z) = x + y + z
   in map sum3 threes
module Day06 where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 5934, P solveB 26984457539)

type Coord = (Int, Int)

solve :: [String] -> Int -> Int
solve textLines days =
  let fish = map read $ splitOn "," $ head textLines :: [Int]
      daysWithPlaceholders = map length $ group $ sort (fish ++ [1 .. 9]) -- Includes an additionall placeholder fish per day.
      initalDays = map (subtract 1) daysWithPlaceholders -- Subtract placeholders.
      finalDays = iterate advance initalDays !! (days - 1)
   in sum finalDays

solveA :: [String] -> Int
solveA textLines = solve textLines 80

solveB :: [String] -> Int
solveB textLines = solve textLines 256

advance :: [Int] -> [Int]
advance [] = error "Fish shouldn't die."
advance (today : rest) = take 6 rest ++ [rest !! 6 + today, rest !! 7, today]
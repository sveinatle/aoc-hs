module Day02 where

import Data.Char
import Data.List
import DayProblem
import Debug.Trace

log2 v = trace (show v) v

cases = [Case solveA "Test" 15, Problem solveA "Problem", Case solveB "Test" 9999, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines = sum $ map (calculateScore . readRound) lines

calculateScore :: (Int, Int) -> Int
calculateScore (them, us) = case (3 + us - them) `mod` 3 of
  0 -> 3 + us
  1 -> 6 + us
  2 -> 0 + us
  _ -> error "unexpected diff"

readRound :: [Char] -> (Int, Int)
readRound [them, _, us] = (1 + ord them - ord 'A', 1 + ord us - ord 'X')
readRound _ = error "unexpected input format"

solveB :: [String] -> Int
solveB lines = 0
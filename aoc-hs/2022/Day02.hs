module Day02 where

import Data.Char
import Data.List
import DayProblem
import Debug.Trace

log2 v = trace (show v) v

cases = [Case solveA "Test" 15, Problem solveA "Problem", Case solveB "Test" 12, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines = sum $ map (calculateScore . readRound selectHandIdentity) lines

-- Normalize to 1-3
normalize v = ((v - 1) `mod` 3) + 1

calculateScore :: (Int, Int) -> Int
calculateScore (them, us) = case (3 + us - them) `mod` 3 of
  0 -> 3 + us -- Tie
  1 -> 6 + us -- Win
  2 -> 0 + us -- Lose
  _ -> error "unexpected diff"

readRound :: (Int -> Int -> Int) -> [Char] -> (Int, Int)
readRound handSelector [themChar, _, usChar] =
  let them = 1 + ord themChar - ord 'A'
      us = 1 + ord usChar - ord 'X'
   in (them, handSelector them us)
readRound handSelector _ = error "unexpected input format"

selectHandIdentity them us = us

selectHandFromOutcome them outcome = case outcome of
  1 -> normalize (them - 1) -- Lose
  2 -> them -- Tie
  3 -> normalize (them + 1) -- Win
  _ -> error "unexpected input"

solveB :: [String] -> Int
solveB lines = sum $ map (calculateScore . readRound selectHandFromOutcome) lines
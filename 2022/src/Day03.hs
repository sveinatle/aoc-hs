module Day03 where

import Data.Char
import Data.List
import DayProblem
import Debug.Trace

log2 v = trace (show v) v

cases = [Case solveA "Test" 157, Problem solveA "Problem", Case solveB "Test" 70, Problem solveB "Problem"]

splitBag :: [a] -> ([a], [a])
splitBag items =
  let count = length items
      countPerRoom = count `div` 2
   in splitAt countPerRoom items

getDuplicate :: [Char] -> Char
getDuplicate = head . uncurry intersect . splitBag

scoreCharacter :: Char -> Int
scoreCharacter c = if c `elem` ['a' .. 'z'] then 1 + ord c - ord 'a' else 27 + ord c - ord 'A'

solveA :: [String] -> Int
solveA lines = sum $ map (scoreCharacter . getDuplicate) lines

solveB :: [String] -> Int
solveB lines = 0
module Day03 where

import Data.Char
import Data.Foldable (Foldable (foldr'))
import Data.List
import Data.List.Split (chunksOf)
import DayProblem
import Debug.Trace
import GHC.Real (reduce)

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
solveA = sum . map (scoreCharacter . getDuplicate)

groupBy3 :: [e] -> [[e]]
groupBy3 = chunksOf 3

findCommon :: Eq a => [[a]] -> [a]
findCommon = foldr1 intersect

solveB :: [String] -> Int
solveB = sum . map (scoreCharacter . head . findCommon) . groupBy3
module Day06 where

import Data.Char
import Data.Foldable (Foldable (foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn)
import DayProblem
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

dbg v = trace (show v) "debug"

cases =
  [ Case solveA "TestA" 7,
    Case solveA "TestB" 5,
    Case solveA "TestC" 6,
    Case solveA "TestD" 10,
    Case solveA "TestE" 11,
    Problem solveA "Problem",
    Case solveB "TestA" 19,
    Case solveB "TestB" 23,
    Case solveB "TestC" 23,
    Case solveB "TestD" 29,
    Case solveB "TestE" 26,
    Problem solveB "Problem"
  ]

solveA :: [String] -> Int
solveA lines = searchForMarker 4 0 (head lines)

searchForMarker :: Int -> Int -> [Char] -> Int
searchForMarker markerLength startIdx characters =
  if ((== markerLength) . length . nub . sort . take markerLength . drop startIdx) characters
    then startIdx + markerLength
    else searchForMarker markerLength (startIdx + 1) characters

solveB :: [String] -> Int
solveB lines = searchForMarker 14 0 (head lines)
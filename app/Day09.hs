module Day09 where

import Data.Char (digitToInt)
import Data.List (transpose)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 15, P solveB 61229)

solveA :: [String] -> Int
solveA lines =
  let rows = map (map digitToInt) lines
      columns = transpose rows
      rowBoolsFlattened = concatMap findMinimums rows
      columnBoolsFlattened = concat $ transpose $ map findMinimums columns
      intersectedBoolsFlattened = zipWith (&&) rowBoolsFlattened columnBoolsFlattened
      rowsFlattened = concat rows
      minimumValues = map snd $ filter fst (zip intersectedBoolsFlattened rowsFlattened)
   in risk minimumValues

risk :: [Int] -> Int
risk points = sum $ map (+ 1) points

findMinimums :: [Int] -> [Bool]
findMinimums values =
  let pairs = zip values (tail values)
      smallest1 = map (uncurry (<)) pairs ++ [True]
      smallest2 = True : map (uncurry (>)) pairs
   in zipWith (&&) smallest1 smallest2

solveB :: [String] -> Int
solveB lines = 9

trace' msg value = trace (msg ++ " " ++ show value) value

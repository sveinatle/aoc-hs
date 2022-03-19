module Day14 where

import Data.List (find, group, intercalate, nub, sort, stripPrefix, transpose)
import Data.List.Split (chunk, chunksOf, splitOn)
import Data.Tuple (swap)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 1588, P solveB 2188189693529)

solveA :: [String] -> Int
solveA lines = solve lines 10

solveB :: [String] -> Int
solveB lines = 999 --solve lines 40
-- cache pairs somehow?

solve lines iterations =
  let initial = head lines
      rules = map readRule $ (tail . tail) lines
      result = iterate (iterator rules) initial !! iterations
      charCounts = (map length . group . sort) result
   in maximum charCounts - minimum charCounts

readRule :: [Char] -> ((Char, Char), Char)
readRule line =
  let [[a, b], [to]] = splitOn " -> " line
   in ((a, b), to)

iterator :: [((Char, Char), Char)] -> [Char] -> [Char]
iterator rules input =
  let pairs = zip input (tail input)
      applyRules pair output = case find ((== pair) . fst) rules of
        Just ((a, b), x) -> a : x : output
        Nothing -> fst pair : output
   in foldr applyRules [last input] pairs

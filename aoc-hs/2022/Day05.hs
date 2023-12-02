module Day05 where

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

cases = [CaseStr solveA "Test" "CMZ", ProblemStr solveA "Problem", CaseStr solveB "Test" "MCD", ProblemStr solveB "Problem"]

solveA :: [String] -> String
solveA lines =
  let (stacks, moves) = readProblem lines
   in map head $ foldl applyMultiMove stacks moves

readProblem :: [String] -> ([[Char]], [(Int, Int, Int)])
readProblem lines =
  let [stackLines, moveLines] = splitOn [""] lines
      stacks = readStacks stackLines
      moves = readMoves moveLines
   in (stacks, moves)

applyMultiMove :: [[Char]] -> (Int, Int, Int) -> [[Char]]
applyMultiMove stacks (count, from, to) = foldl applySingleMove stacks (replicate count (from, to))

applySingleMove :: [[Char]] -> (Int, Int) -> [[Char]]
applySingleMove stacks (from, to) =
  let (move : fromStack) = stacks !! from
      toStack = (move : stacks !! to)
   in replaceListElement from fromStack $
        replaceListElement to toStack stacks

applyBatchMove :: [[Char]] -> (Int, Int, Int) -> [[Char]]
applyBatchMove stacks (count, from, to) =
  let (move, fromStack) = splitAt count (stacks !! from)
      toStack = (move ++ stacks !! to)
   in replaceListElement from fromStack $
        replaceListElement to toStack stacks

replaceListElement :: Int -> [Char] -> [[Char]] -> [[Char]]
replaceListElement idx newElement list = take idx list ++ [newElement] ++ drop (idx + 1) list

readStacks :: [String] -> [[Char]]
readStacks = map (reverse . tail . dropWhileEnd (== ' ')) . filter (isDigit . head) . transpose . reverse

readMoves :: [String] -> [(Int, Int, Int)]
readMoves lines =
  let readMatch (Just [n, from, to]) = (read n, read from - 1, read to - 1)
      readMatch _ = error "Failed to parse move"
   in map (readMatch . matchRegex (mkRegex "[^0-9]*([0-9]+)[^0-9]*([0-9]+)[^0-9]*([0-9]+).*")) lines

solveB :: [String] -> String
solveB lines =
  let (stacks, moves) = readProblem lines
   in map head $ foldl applyBatchMove stacks moves
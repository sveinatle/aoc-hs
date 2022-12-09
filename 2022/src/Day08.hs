{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day08 where

import Data.Char
import Data.Foldable (Foldable (foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import DayProblem (Case (Case, Problem))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

trace'' txt v = trace (txt ++ show v) v

cases =
  [ Case solveA "Test" 21,
    Problem solveA "Problem",
    Case solveB "Test" 8,
    Problem solveB "Problem"
  ]

readProblem :: [[Char]] -> [[Int]]
readProblem = map (map (\c -> ord c - ord '0'))

markVisible :: [Int] -> [Bool]
markVisible trees = snd $ mapAccumL (\tallest this -> (max tallest this, this > tallest)) (-1) trees

solveA :: [String] -> Int
solveA lines =
  let treeRows = readProblem lines
      left = map markVisible treeRows
      right = map (reverse . markVisible . reverse) treeRows
      top = transpose $ map markVisible $ transpose treeRows
      bottom = transpose $ map (reverse . markVisible . reverse) $ transpose treeRows
   in length $ filter (== True) $ map (\(l, r, t, b) -> l || r || t || b) $ zip4 (concat left) (concat right) (concat top) (concat bottom)

distanceTwiceSingle :: [Int] -> Int -> Int
distanceTwiceSingle trees idx =
  let (left, self : right) = splitAt idx trees
      visibleTrees trees height = case break (>= height) trees of
        (visible, rest) -> length visible + if null rest then 0 else 1
   in visibleTrees (reverse left) self * visibleTrees right self

solveB :: [String] -> Int
solveB lines =
  let treeRows = readProblem lines
      distanceTwiceList trees = map (distanceTwiceSingle trees) (take (length trees) [0, 1 ..])
      leftright = map distanceTwiceList treeRows
      updown = transpose $ map distanceTwiceList (transpose treeRows)
   in maximum $ zipWith (*) (concat leftright) (concat updown)
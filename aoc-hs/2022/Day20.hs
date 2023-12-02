{-# LANGUAGE TupleSections #-}

module Day20 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import qualified Data.Foldable as HashSet
import qualified Data.HashSet as HashSet
import Data.Ix
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.List.Split as V
import Data.Maybe (isJust)
import Data.String (IsString)
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

t0 v = trace (show v) 0

t' txt v = trace (txt ++ show v) v

cases =
  [ Case solveA "Test" 3,
    Problem solveA "Problem",
    Case solveB "Test" 1623178306,
    Problem solveB "Problem"
  ]

mix :: [(Int, Int)] -> [(Int, Int)]
mix numsWithOrigIndex = foldl mixNum numsWithOrigIndex [0 .. (length numsWithOrigIndex - 1)]

mixNum :: [(Int, Int)] -> Int -> [(Int, Int)]
mixNum numsWithOrigIndex origIdxToMove = case findIndex ((==) origIdxToMove . fst) numsWithOrigIndex of
  Just sourceIdx ->
    let value = snd $ numsWithOrigIndex !! sourceIdx
        without = take sourceIdx numsWithOrigIndex ++ drop (sourceIdx + 1) numsWithOrigIndex
        targetIdx = (sourceIdx + value) `mod` length without
     in take targetIdx without ++ (origIdxToMove, value) : drop targetIdx without
  Nothing -> error "Failed to find next."

getResult :: [Int] -> Int
getResult nums = case elemIndex 0 nums of
  Just zeroIdx ->
    let takeNthAfter0 n = (nums !! ((zeroIdx + n) `mod` length nums))
     in takeNthAfter0 1000 + takeNthAfter0 2000 + takeNthAfter0 3000
  Nothing -> error "0 not found"

solveA :: [String] -> Int
solveA lines =
  let numsWithOrigIndex = zip [0 .. (length lines - 1)] (map read lines)
      mixed = mix numsWithOrigIndex
   in getResult (map snd mixed)

solveB :: [String] -> Int
solveB lines =
  let numsWithOrigIndex = zip [0 .. (length lines - 1)] $ map ((*) 811589153 . read) lines
      mixed = iterate mix numsWithOrigIndex !! 10
   in getResult (map snd mixed)

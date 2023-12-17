{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day09 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, find, group, groupBy, intersect, maximumBy, minimumBy, nub, sort, sortBy, transpose)
import Data.List.Split (chunksOf, splitEvery, splitOn, splitOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 114, Problem solveA "Problem", Case solveB "Test" 2, Problem solveB "Problem"]

readProblem :: [String] -> [[Int]]
readProblem = map readLine
  where
    readLine :: String -> [Int]
    readLine = map read . words

predict :: [Int] -> (Int, Int)
predict nums
  | all (== 0) nums = (0, 0)
  | otherwise =
    let diffs = zipWith (\l r -> r - l) nums (drop 1 nums)
        (nextStartDiff, nextEndDiff) = predict diffs
     in (head nums - nextStartDiff, last nums + nextEndDiff)

solveA :: [String] -> Int
solveA = sum . map (snd . predict) . readProblem

solveB :: [String] -> Int
solveB = sum . map (fst . predict) . readProblem
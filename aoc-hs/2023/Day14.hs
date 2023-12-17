{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, elemIndices, find, group, groupBy, intercalate, intersect, maximumBy, minimumBy, nub, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (chunksOf, splitEvery, splitOn, splitOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 136, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

process :: String -> Int
process column = case foldl
  ( \(nextRowWeight, nextShiftedWeight, totalWeight) c ->
      case c of
        'O' -> (nextRowWeight -1, nextShiftedWeight -1, totalWeight + nextShiftedWeight)
        '#' -> (nextRowWeight -1, nextRowWeight -1, totalWeight)
        '.' -> (nextRowWeight -1, nextShiftedWeight, totalWeight)
        _ -> error "Unexpected char."
  )
  (topWeight, topWeight, 0)
  column of
  (_, _, total) -> total
  where
    topWeight = length column

solveA :: [String] -> Int
solveA = sum . map process . transpose

solveB :: [String] -> Int
solveB lines = 0
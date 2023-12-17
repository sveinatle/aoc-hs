{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day13 where

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

cases = [Case solveA "Test" 405, Problem solveA "Problem", Case solveB "Test" 400, Problem solveB "Problem"]

type Pattern = [String]

findReflection :: [String] -> (Int, Int)
findReflection rows = case findReflectionOneWay rows of
  Just reflection -> (reflection, 0)
  Nothing -> case findReflectionOneWay (transpose rows) of
    Just reflection -> (0, reflection)
    Nothing -> error "No reflection found"
  where
    findReflectionOneWay :: [String] -> Maybe Int
    findReflectionOneWay rows =
      let potentials = map findCandidateIndices rows
       in case foldl1 intersect potentials of
            [] -> Nothing
            [reflection] -> Just reflection
            _ -> error "Multiple reflections."

    findCandidateIndices :: String -> [Int]
    findCandidateIndices row =
      let rowSplits = map (`splitAt` row) [1 .. length row -1]
       in map (length . fst) . filter compareParts $ rowSplits
      where
        compareParts (l, r) =
          let len = min (length l) (length r)
           in take len (reverse l) == take len r

solveA :: [String] -> Int
solveA = sum . map ((\(x, y) -> x + y * 100) . findReflection) . splitOn [""]

solveB :: [String] -> Int
solveB lines = 0
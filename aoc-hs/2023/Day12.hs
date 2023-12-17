{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

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
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 21, Problem solveA "Problem", Case solveB "Test" 525152, Problem solveB "Problem"]

data SpringStatus = Unknown | Operational | Damaged

instance Eq SpringStatus where
  (==) Operational Damaged = False
  (==) Damaged Operational = False
  (==) _ _ = True

instance Show SpringStatus where
  show Operational = "."
  show Damaged = "#"
  show Unknown = "?"

readRow :: [Char] -> ([SpringStatus], [Int])
readRow line =
  let [springsStr, groupsStr] = words line
   in (map readSpring springsStr, map read (splitOn "," groupsStr))
  where
    readSpring '?' = Unknown
    readSpring '.' = Operational
    readSpring '#' = Damaged
    readSpring _ = error "Unexpected spring status character."

countArrangements :: [SpringStatus] -> [Int] -> Int
countArrangements springsTemplate damagedGroupSizes =
  let springCount = length springsTemplate
      damagedCount = sum damagedGroupSizes
      operationalCount = springCount - damagedCount
      arrangements = generateArrangements damagedGroupSizes operationalCount
      validArrangements = filter (== springsTemplate) arrangements
   in length validArrangements
  where
    generateArrangements :: [Int] -> Int -> [[SpringStatus]]
    generateArrangements damagedGroupSizes operationalCount =
      let operationalGroupsList = filter (not . hasInternalZero) $ generateOperationalGroups (length damagedGroupSizes + 1) operationalCount
       in map (generateArrangement damagedGroupSizes) operationalGroupsList

    generateOperationalGroups :: Int -> Int -> [[Int]]
    generateOperationalGroups 0 _ = [[]]
    generateOperationalGroups remainingGroupCount remainingSpringCount =
      [ g : groups
        | g <- [0 .. remainingSpringCount],
          groups <- generateOperationalGroups (remainingGroupCount - 1) (remainingSpringCount - g),
          sum (g : groups) == remainingSpringCount
      ]

    generateArrangement :: [Int] -> [Int] -> [SpringStatus]
    generateArrangement damagedGroupSizes operationalGroupSizes = concat . concatMap (\(a, b) -> [replicate a Damaged, replicate b Operational]) $ zip (0 : damagedGroupSizes) operationalGroupSizes

    hasInternalZero :: [Int] -> Bool
    hasInternalZero nums = elem 0 . drop 1 . take (length nums - 1) $ nums

--showComparison a t = concatMap show a ++ "\n" ++ concatMap show t ++ show (a == t)

solveA :: [String] -> Int
solveA = sum . map (uncurry countArrangements . readRow)

solveB :: [String] -> Int
solveB = sum . map (uncurry countArrangements . multiplyRow . readRow)
  where
    multiplyRow :: ([SpringStatus], [Int]) -> ([SpringStatus], [Int])
    multiplyRow (springs, groups) = (intercalate [Unknown] (replicate 5 springs), concat $ replicate 5 groups)

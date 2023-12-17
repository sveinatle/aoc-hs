{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, elemIndices, find, group, groupBy, intersect, maximumBy, minimumBy, nub, permutations, sort, sortBy, transpose, (\\))
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

cases = [Case solveA "Test" 374, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

type Galaxy = (Int, Int)

readGalaxies :: [String] -> [Galaxy]
readGalaxies = concatMap (\(y, xs) -> map (,y) xs) . zip [0 ..] . map readXs
  where
    readXs :: [Char] -> [Int]
    readXs = elemIndices '#'

findGaps :: [Int] -> [Int]
findGaps nums = [(minimum nums) .. (maximum nums)] \\ nums

calcDistance :: [Int] -> [Int] -> (Galaxy, Galaxy) -> Int
calcDistance xGaps yGaps ((ax, ay), (bx, by)) = calcDistance1 xGaps ax bx + calcDistance1 yGaps ay by
  where
    calcDistance1 gaps a b = abs (a - b) + length (filter (\g -> g > a && g < b || g > b && g < a) gaps)

solveA :: [String] -> Int
solveA lines =
  let galaxies = readGalaxies lines
      xGaps = findGaps $ map fst galaxies
      yGaps = findGaps $ map snd galaxies
   in (`div` 2) . sum . map (calcDistance xGaps yGaps) $ [(a, b) | a <- galaxies, b <- galaxies]

solveB :: [String] -> Int
solveB lines = 0
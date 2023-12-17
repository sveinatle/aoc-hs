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

showRows :: [String] -> String
showRows = intercalate "\n"

findReflections :: [String] -> [(Int, Int)]
findReflections rows = map (,0) (findReflectionsOneWay rows) ++ map (0,) (findReflectionsOneWay (transpose rows))
  where
    findReflectionsOneWay :: [String] -> [Int]
    findReflectionsOneWay rows =
      let potentials = map findCandidateIndices rows
       in foldl1 intersect potentials

    findCandidateIndices :: String -> [Int]
    findCandidateIndices row =
      let rowSplits = map (`splitAt` row) [1 .. length row -1]
       in map (length . fst) . filter compareParts $ rowSplits
      where
        compareParts (l, r) =
          let len = min (length l) (length r)
           in take len (reverse l) == take len r

findReflectionWithSmudge :: [String] -> (Int, Int)
findReflectionWithSmudge rows = case find (/= unsmudgedReflection) (concatMap findReflections (smudgedMirrors rows)) of
  Just reflection -> reflection
  Nothing -> error "No smudged reflection found."
  where
    smudgedMirrors :: [String] -> [[String]]
    smudgedMirrors rows =
      let w = length (head rows)
          h = length rows
       in [smudgeMirror rows (x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]

    smudgeMirror :: [String] -> (Int, Int) -> [String]
    smudgeMirror rows (x, y) =
      let smudge = case (rows !! y) !! x of
            '#' -> '.'
            '.' -> '#'
            _ -> error "Unexpected character."
          newRow = replaceElement (rows !! y) x smudge
       in replaceElement rows y newRow

    replaceElement :: [a] -> Int -> a -> [a]
    replaceElement list idx replacement =
      let before = take idx list
          after = drop (idx + 1) list
       in before ++ [replacement] ++ after

    unsmudgedReflection :: (Int, Int)
    unsmudgedReflection = case findReflections rows of
      [reflection] -> reflection
      _ -> error "Unsmudged error."

solveA :: [String] -> Int
solveA = sum . map ((\(x, y) -> x + y * 100) . head . findReflections) . splitOn [""]
  where
    takeJust (Just x, Nothing) = (x, 0)
    takeJust (Nothing, Just y) = (0, y)
    takeJust (Just x, Just y) = error "Multiple reflections found"
    takeJust _ = error "No reflection found"

solveB :: [String] -> Int
solveB = sum . map ((\(x, y) -> x + y * 100) . findReflectionWithSmudge) . splitOn [""]
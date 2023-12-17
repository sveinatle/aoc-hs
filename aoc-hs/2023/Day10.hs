{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day10 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, find, group, groupBy, intersect, maximumBy, minimumBy, nub, sort, sortBy, transpose)
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

cases = [Case solveA "Test" 8, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

type Coord = (Int, Int)

-- TODO: Reprecent as arrays of possible directions? Might make deduceStartCell simpler.
data Cell = S | UD | LR | DL | DR | UL | UR | G deriving (Eq, Show)

type Grid = Vector (Vector Cell)

readProblem :: [String] -> Grid
readProblem = V.fromList . map readLine
  where
    readLine :: String -> Vector Cell
    readLine = V.fromList . map readCell

    readCell :: Char -> Cell
    readCell 'S' = S
    readCell '-' = LR
    readCell '|' = UD
    readCell '7' = DL
    readCell 'F' = DR
    readCell 'J' = UL
    readCell 'L' = UR
    readCell '.' = G
    readCell _ = error "Unexpected cell character."

getCell :: Grid -> Coord -> Cell
getCell grid (x, y) =
  fromMaybe
    G
    ( do
        row <- grid V.!? y
        row V.!? x
    )

findStart :: Grid -> Coord
findStart grid =
  let rowResults = V.map (V.findIndex (== S)) grid
      y = case V.findIndex isJust rowResults of
        Just y -> y
        Nothing -> error "Start row not found."
      x = case rowResults V.! y of
        Just x -> x
        Nothing -> error "Start col not found."
   in (x, y)

deduceStartCell :: Grid -> Coord -> Cell
deduceStartCell grid (x, y) =
  let u = getCell grid (x, y -1) `elem` [UD, DL, DR]
      d = getCell grid (x, y + 1) `elem` [UD, UL, UR]
      l = getCell grid (x -1, y) `elem` [LR, UR, DR]
      r = getCell grid (x + 1, y) `elem` [LR, UL, DL]
   in case (u, d, l, r) of
        (False, False, True, True) -> LR
        (True, True, False, False) -> UD
        (False, True, True, False) -> DL
        (False, True, False, True) -> DR
        (True, False, True, False) -> UL
        (True, False, False, True) -> UR
        _ -> error "Unable to deduce start cell type."

getConnectedCoords :: Coord -> Cell -> [Coord]
getConnectedCoords (x, y) cell = map (\(dx, dy) -> (x + dx, y + dy)) $ case cell of
  LR -> [(-1, 0), (1, 0)]
  UD -> [(0, -1), (0, 1)]
  DL -> [(0, 1), (-1, 0)]
  DR -> [(0, 1), (1, 0)]
  UL -> [(0, -1), (-1, 0)]
  UR -> [(0, -1), (1, 0)]
  _ -> error $ "Cannot navigate from " ++ show cell

navigate :: Grid -> [(Coord, Cell)] -> [(Coord, Cell)]
navigate grid [] = error "Unexpected empty path."
navigate grid path@((_, S) : history) =
  -- Reached start point. Return accumulated path.
  path
navigate grid path@(((coord, cell) : (prevCoord, prevCell) : history)) =
  -- When we have history (not at start point), we'll filter out the cell we came from.
  let nextCoords = getConnectedCoords coord cell
      nextCoord = head $ filter (/= prevCoord) nextCoords -- Exclude coord we came from.
      nextCell = getCell grid nextCoord
   in navigate grid ((nextCoord, nextCell) : path)
navigate grid path@(((coord, cell) : history)) =
  --When we don't have history (at start), just take a random of the two next candidates.
  let nextCoords = getConnectedCoords coord cell
      nextCoord = head nextCoords -- Start cell will have two candidates, just take first one.
      nextCell = getCell grid nextCoord
   in navigate grid ((nextCoord, nextCell) : path)

solveA :: [String] -> Int
solveA lines = (`div` 2) . length . navigate grid $ [(startCoord, startCell)]
  where
    grid = readProblem lines
    startCoord = findStart grid
    startCell = deduceStartCell grid startCoord

solveB :: [String] -> Int
solveB ls = 0
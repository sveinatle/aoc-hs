{-# LANGUAGE TupleSections #-}

module Day15 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
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

tA v = trace (intercalate "\n" $map show v) v

cases =
  [ Case solveATest "Test" 26,
    Problem solveAProb "Problem",
    Case solveBTest "Test" 56000011,
    Problem solveBProb "Problem"
  ]

type SensorBeacon = ((Int, Int), (Int, Int))

type Coverage = (Int, Int)

readSensor :: String -> SensorBeacon
readSensor line =
  let Just [sx, sy, bx, by] = matchRegex (mkRegex "[^-0-9]*([-0-9]*)[^-0-9]*([-0-9]*)[^-0-9]*([-0-9]*)[^-0-9]*([-0-9]*)[^-0-9]*") line
   in ((read sx, read sy), (read bx, read by))

sensorCoverage :: Int -> SensorBeacon -> Maybe Coverage
sensorCoverage row ((sx, sy), (bx, by)) =
  let reach = abs (sx - bx) + abs (sy - by)
      rowDistance = abs (sy - row)
      remainingX = reach - rowDistance
   in if remainingX >= 0 then Just (sx - remainingX, sx + remainingX) else Nothing

getCoverageForRow :: [SensorBeacon] -> Int -> (Int, [Coverage])
getCoverageForRow sensors row =
  let coverages = (normalizeCoverages . map (\(Just v) -> v) . filter isJust . map (sensorCoverage row)) sensors
   in (row, coverages)

normalizeCoverage :: [Coverage] -> Coverage -> [Coverage]
normalizeCoverage [] c = [c]
normalizeCoverage ((from1, to1) : acc) (from2, to2) =
  if from2 <= to1 + 1
    then (from1, max to1 to2) : acc -- Overlap, extend with new range if possible.
    else (from2, to2) : (from1, to1) : acc -- No overlap, append as new range.

normalizeCoverages :: [Coverage] -> [Coverage]
normalizeCoverages = foldl normalizeCoverage [] . sort

countPositions :: [Coverage] -> Int
countPositions coverages = sum $ map (\(from, to) -> 1 + to - from) coverages

solveA :: Int -> [String] -> Int
solveA row lines =
  let sensors = map readSensor lines
      coverages = snd $ getCoverageForRow sensors row
      beaconXsInRow = nub $ sort $ (map snd . filter (\(bx, by) -> by == row) . map snd) sensors
      isBeaconXInCoverage bx = any (\(from, to) -> from <= bx && bx <= to) coverages
      beaconInCoverageCount = length $ filter isBeaconXInCoverage beaconXsInRow
   in (subtract beaconInCoverageCount . countPositions) coverages

solveATest = solveA 10

solveAProb = solveA 2000000

solveB :: Int -> [String] -> Int
solveB maxRow lines =
  let sensors = map readSensor lines
      splitCoverages = filter (\(row, coverages) -> length coverages >= 2) $ map (getCoverageForRow sensors) [0 .. maxRow]
   in case splitCoverages of
        [(row, [(from1, to1), (from2, to2)])] ->
          let x = min to1 to2 + 1
           in x * 4000000 + row
        [] -> error "No gaps found."
        _ -> error "Found more than one hole in row."

solveBTest = solveB 20

solveBProb = solveB 4000000

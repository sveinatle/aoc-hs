{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day06 where

import Data.Char (isDigit, isSpace)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, find, groupBy, intersect, nub, sort, sortBy, transpose)
import Data.List.Split (chunksOf, splitEvery, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [CaseStr solveA "Test" "288", ProblemStr solveA "Problem", CaseStr solveB "Test" "71503", ProblemStr solveB "Problem"]

data Race = Race {time :: Double, dist :: Double} deriving (Show)

-- hold + drive = time
-- dist = drive * hold
--
-- hold = time - drive
--
-- dist < drive * (time - drive)
-- 0 < -1 * drive^2 + time * drive - dist
-- ((-b) +- sqrt(b*b - 4ac)) / 2a
-- ((-time) +- sqrt(time*time - 4*(-1)*(-dist))) / (2*(-1))
-- ((-time) +- sqrt(time*time - 4*dist)) / (-2)

solveA :: [String] -> String
solveA = show . product . map (getLeeway . computeHoldRange) . readProblem
  where
    readProblem :: [String] -> [Race]
    readProblem = map (\[time, dist] -> Race {..}) . transpose . map (map read . drop 1 . words)

computeHoldRange :: Race -> (Integer, Integer)
computeHoldRange Race {..} =
  let x1 = ((- time) + sqrt (time * time - 4 * dist)) / (-2)
      x2 = ((- time) - sqrt (time * time - 4 * dist)) / (-2)
      lowerRecord = min x1 x2
      upperRecord = max x1 x2
      lowerRecordRounded = ceiling lowerRecord
      upperRecordRounded = floor upperRecord
      lowerWin = if fromIntegral lowerRecordRounded == lowerRecord then lowerRecordRounded + 1 else lowerRecordRounded
      upperWin = if fromIntegral upperRecordRounded == upperRecord then upperRecordRounded - 1 else upperRecordRounded
   in (lowerWin, upperWin)

getLeeway :: (Integer, Integer) -> Integer
getLeeway (minHold, maxHold) = maxHold + 1 - minHold

solveB :: [String] -> String
solveB = show . getLeeway . computeHoldRange . readProblem
  where
    readProblem :: [String] -> Race
    readProblem lines =
      let [time, dist] = map (read . concat . drop 1 . words) lines
       in Race {..}
{-# LANGUAGE TupleSections #-}

module Day17 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, nub, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)
import Numeric (readHex)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

cases =
  [ Case solveA "Test" 45,
    Problem solveA "Problem",
    Case solveB "Test" 112,
    Problem solveB "Problem"
  ]

readTarget :: [String] -> (Int, Int, Int, Int)
readTarget lines = case matchRegex (mkRegex "x=([0-9-]+)..([0-9-]+), y=([0-9-]+)..([0-9-]+)") (head lines) of
  Just [xMin, xMax, yMax, yMin] -> (read xMin, read xMax, read yMin, read yMax)
  _ -> error "Unexpected input."

solveA :: [String] -> Int
solveA lines =
  let maxYSpeed = maximum $ map (\(_, ySpeed, _) -> ySpeed) $ getCandidates $ readTarget lines
      maxHeight = ((maxYSpeed + 1) * ((maxYSpeed + 1) - 1) `div` 2)
   in maxHeight

solveB :: [String] -> Int
solveB lines = length $ nub $ map (\(x, y, s) -> (x, y)) $ getCandidates $ readTarget lines

trace' :: Show a => a -> a
trace' value = trace (show value) value

trace'' :: Show a => String -> a -> a
trace'' msg value = trace (msg ++ " " ++ show value) value

type StepCount = Int

type XSpeed = Int

type YSpeed = Int

type XPos = Int

type YPos = Int

{-
ySpeedInitial
ySpeedInitial-1
ySpeedInitial-2
ySpeedInitial-3
ySpeedInitial-4
ySpeedInitial-5

(steps*(steps-1)/2)

y after steps steps for a given ySpeedInitial:
steps * ySpeedInitial - (steps*(steps-1)/2) = yPos
steps * ySpeedInitial - steps*steps/2 + steps/2 = yPos
steps * ySpeedInitial + steps/2  - steps*steps/2 = yPos
steps * (ySpeedInitial+1/2)  - steps*steps/2 = yPos
- steps * (ySpeedInitial+1/2)  + steps*steps/2 = - yPos
steps*steps/2 - steps * (ySpeedInitial+1/2) + yPos = 0

a=1/2
b=-(ySpeedInitial+1/2)
c=yPos
steps = (-b +- sqrt(b*b-4*a*c)) / (2*a)

integers between (steps for ymin) and (steps for ymax) are candidate step counts.
only need the + part of +-.
-}
stepCountForTargetYPos :: YSpeed -> YPos -> Float
stepCountForTargetYPos initialSpeed targetPos =
  let a = 0.5
      b = - fromIntegral initialSpeed - 0.5
      c = fromIntegral targetPos
   in (- b + sqrt (b * b - 4 * a * c)) / (2 * a)

getStepCountsForYSpeed :: YSpeed -> YSpeed -> YSpeed -> (YSpeed, [StepCount])
getStepCountsForYSpeed targetYMin targetYMax initialSpeed =
  let minStepCount = ceiling $ stepCountForTargetYPos initialSpeed targetYMin
      maxStepCount = floor $ stepCountForTargetYPos initialSpeed targetYMax
   in (initialSpeed, [minStepCount .. maxStepCount])

getYPos :: Int -> Int -> Int
getYPos stepCount ySpeed = ySpeed * stepCount - (stepCount * (stepCount -1) `div` 2)

getYSpeedRange :: YPos -> [YSpeed]
getYSpeedRange targetYMax = [targetYMax .. (- (targetYMax + 1))]

getYCandidates :: YPos -> YPos -> [(YSpeed, [StepCount])]
getYCandidates targetYMin targetYMax = map (getStepCountsForYSpeed targetYMin targetYMax) $ getYSpeedRange targetYMax

{-
xSpeedInitial
xSpeedInitial-1
xSpeedInitial-2
xSpeedInitial-3
xSpeedInitial-4
0
0

xTargetMin = steps * xSpeedInitial - (steps*(steps-1)/2)
xTargetMin + (steps*(steps-1)/2) = steps * xSpeedInitial
(xTargetMin + (steps*(steps-1)/2)) / steps = xSpeedInitial

xPosMax = xSpeedInitial*xSpeedInitial - ()
-}
getFinalXPos :: Int -> Int
getFinalXPos xSpeed = xSpeed * xSpeed - (xSpeed * (xSpeed -1) `div` 2)

getXPos :: Int -> Int -> Int
getXPos stepCount xSpeed =
  if stepCount >= xSpeed
    then getFinalXPos xSpeed
    else xSpeed * stepCount - (stepCount * (stepCount -1) `div` 2)

getXSpeeds :: XPos -> XPos -> [XSpeed]
getXSpeeds xTargetMin xTargetMax = filter ((>= xTargetMin) . getFinalXPos) [1 .. getMaximumXSpeed xTargetMax]

getMaximumXSpeed :: XPos -> XSpeed
getMaximumXSpeed xTargetMax = xTargetMax

getXSpeedsForStepCount :: XPos -> XPos -> StepCount -> [XSpeed]
getXSpeedsForStepCount xTargetMin xTargetMax stepCount =
  let candidateXSpeeds = getXSpeeds xTargetMin xTargetMax
      inRange min max v = v >= min && v <= max
      validSpeeds = filter (inRange xTargetMin xTargetMax . getXPos stepCount) candidateXSpeeds
   in validSpeeds

-- Combine X and Y:

flattenYCandidates :: [(YSpeed, [StepCount])] -> [(YSpeed, StepCount)]
flattenYCandidates candidates =
  let flattenPair (ySpeed, stepCounts) = map (ySpeed,) stepCounts
   in concatMap flattenPair candidates

flattenXCandidates :: [([XSpeed], YSpeed, StepCount)] -> [(XSpeed, YSpeed, StepCount)]
flattenXCandidates candidates =
  let flattenTri (xSpeeds, ySpeed, stepCount) = map (,ySpeed,stepCount) xSpeeds
   in concatMap flattenTri candidates

getCandidates :: (XPos, XPos, YPos, YPos) -> [(XSpeed, YSpeed, StepCount)]
getCandidates (targetXMin, targetXMax, targetYMin, targetYMax) =
  let addXSpeeds (ySpeed, stepCount) = (getXSpeedsForStepCount targetXMin targetXMax stepCount, ySpeed, stepCount)
   in flattenXCandidates $ map addXSpeeds $ flattenYCandidates $ getYCandidates targetYMin targetYMax

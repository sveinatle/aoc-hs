module Day07 where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA' 37, P solveB 168)

data State = S
  { sPosition :: Int,
    sLeftCrabCount :: Int,
    sHereCrabCount :: Int,
    sRightCrabCount :: Int,
    sTotalDistance :: Int
  }

solveA :: [String] -> Int
solveA lines =
  let crabs = sort $ map read (splitOn "," $ head lines) :: [Int]
      positions = group crabs
      firstPositionCrabs = head positions
      firstPosition = head firstPositionCrabs
      firstPositionCrabCount = length firstPositionCrabs
      firstState =
        S
          { sPosition = firstPosition,
            sLeftCrabCount = 0,
            sHereCrabCount = firstPositionCrabCount,
            sRightCrabCount = length crabs - firstPositionCrabCount,
            sTotalDistance = calculateScore firstPosition crabs
          }
      states = scanl getNextState firstState (tail positions)
   in minimum $ map sTotalDistance states

getNextState :: State -> [Int] -> State
getNextState previousState currentCrabs =
  let currentPosition = head currentCrabs
      currentCrabCount = length currentCrabs
      leftCrabCount = sLeftCrabCount previousState + sHereCrabCount previousState
      rightCrabCount = sRightCrabCount previousState - currentCrabCount
      previousPositionDelta = currentPosition - sPosition previousState
      score = sTotalDistance previousState + (previousPositionDelta * leftCrabCount) - (previousPositionDelta * sRightCrabCount previousState)
   in S
        { sPosition = currentPosition,
          sLeftCrabCount = leftCrabCount,
          sHereCrabCount = currentCrabCount,
          sRightCrabCount = rightCrabCount,
          sTotalDistance = score
        }

calculateScore position crabs = sum $ map (abs . subtract position) crabs

solveA' :: [String] -> Int
solveA' lines =
  let crabs = sort $ map read (splitOn "," $ head lines) :: [Int]
      median crabs = crabs !! (length crabs `div` 2)
   in calculateScore (median crabs) crabs

solveB :: [String] -> Int
solveB lines = 99

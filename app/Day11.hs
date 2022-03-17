module Day11 where

import Data.Char (digitToInt)
import Data.Either (fromLeft)
import Data.List (elemIndex, elemIndices, find, sort, (\\))
import Data.List.Split
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 1656, P solveB 288957)

type Octs = [Int]

type OctCoord = (Int, Int)

data SolutionState = SS Int Octs

data IterationState = IS [Int] Octs

solveA :: [String] -> Int
solveA lines =
  let state = SS 0 $ concatMap (map digitToInt) lines
      result = iterate iterator state !! 100
   in case result of
        SS flashes octs -> flashes

iterator :: SolutionState -> SolutionState
iterator (SS flashes octs) =
  let aged = map (+ 1) octs
      iterationState = flash (IS [] aged)
   in case iterationState of
        IS flashed octs ->
          let totalFlashes = flashes + length flashed
              octsRestarted = map (\v -> if v >= 10 then 0 else v) octs
           in SS totalFlashes octsRestarted

showTable octs = unlines $map show $ chunksOf 10 octs

flash :: IterationState -> IterationState
flash (IS alreadyFlashed octs) =
  let allFlashed = elemIndices True $ map (>= 10) octs
      newFlashed = allFlashed \\ alreadyFlashed
   in if null newFlashed
        then IS allFlashed octs
        else
          let indexToCoord i = (i `mod` 10, i `div` 10)
              newFlashedCoords = map indexToCoord newFlashed
              newOcts = foldr increaseNeighbours octs newFlashedCoords
           in flash (IS allFlashed newOcts)

increaseNeighbours :: OctCoord -> Octs -> Octs
increaseNeighbours oct octs = foldr increaseOct octs (getNeighboursCoords oct)

increaseOct :: OctCoord -> Octs -> Octs
increaseOct oct = updateOct oct (+ 1)

getNeighboursCoords :: OctCoord -> [OctCoord]
getNeighboursCoords (x, y) =
  [ (x', y')
    | x' <- [x -1, x, x + 1],
      y' <- [y -1, y, y + 1],
      valid x'
        && valid y'
        && (x, y) /= (x', y')
  ]
  where
    valid v = v >= 0 && v < 10

updateOct :: OctCoord -> (Int -> Int) -> Octs -> Octs
updateOct (x, y) f octs =
  let i = y * 10 + x
      (pre, value, post) = splitList i octs
   in pre ++ (f value : post)

splitList i l = (take i l, l !! i, drop (i + 1) l)

solveB :: [String] -> Int
solveB lines = 5

trace' msg value = trace (msg ++ " " ++ show value) value
module Day15 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (delete, find, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 40, P solveB 315)

solveA :: [String] -> Int
solveA lines = solve $ map (map digitToInt) lines

solveB :: [String] -> Int
solveB lines =
  let incBy x = map (map (\v -> ((v + x -1) `mod` 9) + 1))
      repeatFiveIncreasing lines = lines ++ incBy 1 lines ++ incBy 2 lines ++ incBy 3 lines ++ incBy 4 lines
   in solve $ transpose $ repeatFiveIncreasing $ transpose $ repeatFiveIncreasing $ map (map digitToInt) lines

trace' :: Show a => a -> a
trace' value = trace (show value) value

solve :: [[Int]] -> Int
solve lines =
  let w = length (head lines)
      h = length lines
      endCell = (w -1, h -1)
      cellCostGrid = concat lines
      emptyState = State (zip [0 ..] $ replicate (w * h) Never) [] 0
      initialState = setCell w (0, 0) (Pending 0) emptyState
      State finalState _ _ = until (\(State grid _ _) -> isFinished $ snd $ getCell grid w endCell) (finishNextCell cellCostGrid w) initialState
   in case snd $ getCell finalState w endCell of
        Finished cost -> cost
        _ -> error "Cell not computed."

data Cell = Never | Pending Int | Finished Int

isPending :: Cell -> Bool
isPending (Pending _) = True
isPending _ = False

isFinished :: Cell -> Bool
isFinished (Finished _) = True
isFinished _ = False

takePending :: Cell -> Int
takePending (Pending v) = v
takePending _ = error "Expected Pending value."

type Grid = [(Int, Cell)]

type PendingCells = [(Int, Int)]

data State = State Grid PendingCells Int

idxToXY w idx = (idx `mod` w, idx `div` w)

getCell :: [a] -> Int -> (Int, Int) -> a
getCell grid w (x, y) = grid !! (y * w + x)

setCell :: Int -> (Int, Int) -> Cell -> State -> State
setCell w (x, y) c state = case state of
  State grid pendingCells finCount ->
    let i = y * w + x
        newPendingCells = case c of
          Pending cost -> (i, cost) : pendingCells
          Finished _ -> filter ((/= i) . fst) pendingCells
          _ -> pendingCells
        newGrid = take i grid ++ [(i, c)] ++ drop (i + 1) grid
     in State newGrid newPendingCells finCount

updateCell :: [Int] -> Int -> Int -> (Int, Int) -> State -> State
updateCell cellCostGrid w pathCost (x, y) (State grid pendingCells finCount)
  | x < 0 || y < 0 || x >= w || y >= length grid `div` w = state
  | otherwise =
    let c = (x, y)
        newPathCost = pathCost + getCell cellCostGrid w c
     in case snd $ getCell grid w c of
          Never -> setCell w c (Pending newPathCost) state
          Pending oldPathCost ->
            if newPathCost < oldPathCost
              then setCell w c (Pending newPathCost) state
              else state
          Finished _ -> state
  where
    state = State grid pendingCells finCount

finishNextCell :: [Int] -> Int -> State -> State
finishNextCell cellCostGrid w (State grid pendingCells finCount) =
  let (idx, cost) = minimumBy (\x y -> compare (snd x) (snd y)) pendingCells
      (x, y) = log $idxToXY w idx
      log v = if finCount `mod` 100 == 0 then trace (show finCount) v else v
   in updateCell cellCostGrid w cost (x + 1, y) $
        updateCell cellCostGrid w cost (x -1, y) $
          updateCell cellCostGrid w cost (x, y + 1) $
            updateCell cellCostGrid w cost (x, y -1) $
              setCell w (x, y) (Finished cost) (State grid pendingCells (finCount + 1))
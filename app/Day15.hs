module Day15 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, permutations, sort, sortBy, transpose, (\\))
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
      isEndCellScoreFinished grid = isFinished $ snd $ gridGetCell grid w endCell
      emptyState = State (zip [0 ..] $ replicate (w * h) Never) [] 0
      initialState = stateSetCell w (0, 0) (Pending 0) emptyState
      State finalState _ _ = until (isEndCellScoreFinished . stateGrid) (finishNextCell cellCostGrid w) initialState
   in case snd $ gridGetCell finalState w endCell of
        Finished cost -> cost
        _ -> error "Cell not computed."

type Cost = Int

type CellCostGrid = [Cost]

type Width = Int

type CellIdx = Int

type CellCoord = (Int, Int)

data Cell = Never | Pending Cost | Finished Cost deriving (Show)

isPending :: Cell -> Bool
isPending (Pending _) = True
isPending _ = False

isFinished :: Cell -> Bool
isFinished (Finished _) = True
isFinished _ = False

type PathCostGrid = [(CellIdx, Cell)]

type PendingCells = [(CellIdx, Cost)]

data State = State {stateGrid :: PathCostGrid, statePendingCells :: PendingCells, stateFinishedCount :: Int}

idxToXY w idx = (idx `mod` w, idx `div` w)

pendingCellsAddCell :: Width -> (CellIdx, Cost) -> PendingCells -> PendingCells
pendingCellsAddCell w (cellIdx, cost) pendingCells =
  let sortedInsertIdx = findIndex (\(i, arrayCost) -> arrayCost >= cost) pendingCells
   in case sortedInsertIdx of
        -- Append to end if no pending cells have higher cost than this cell.
        Nothing -> pendingCells ++ [(cellIdx, cost)]
        -- Otherwise insert into sorted idx.
        Just arrayIdx -> take arrayIdx pendingCells ++ [(cellIdx, cost)] ++ drop arrayIdx pendingCells

gridGetCell :: [a] -> Width -> CellCoord -> a
gridGetCell grid w (x, y) = grid !! (y * w + x)

stateSetCell :: Width -> CellCoord -> Cell -> State -> State
stateSetCell w (x, y) newCellValue (State grid pendingCells finCount) =
  let cellIdx = y * w + x
      newPendingCells = case newCellValue of
        Pending cost -> pendingCellsAddCell w (cellIdx, cost) pendingCells
        _ -> pendingCells
      newGrid = take cellIdx grid ++ [(cellIdx, newCellValue)] ++ drop (cellIdx + 1) grid
   in State newGrid newPendingCells finCount

updateCell :: CellCostGrid -> Width -> Cost -> CellCoord -> State -> State
updateCell cellCostGrid w pathCost (x, y) (State grid pendingCells finCount)
  | x < 0 || y < 0 || x >= w || y >= length grid `div` w = state
  | otherwise =
    let c = (x, y)
        newPathCost = pathCost + gridGetCell cellCostGrid w c
     in case snd $ gridGetCell grid w c of
          Never -> stateSetCell w c (Pending newPathCost) state
          Pending oldPathCost ->
            if newPathCost < oldPathCost
              then stateSetCell w c (Pending newPathCost) state
              else state
          Finished _ -> state
  where
    state = State grid pendingCells finCount

finishNextCell :: CellCostGrid -> Width -> State -> State
finishNextCell cellCostGrid w (State grid pendingCells finCount) =
  let (pendingIdx, pathCost) = head pendingCells
      (x, y) = log $idxToXY w pendingIdx
      log v = if finCount `mod` 100 == 0 then trace (show finCount ++ show pendingCells) v else v
   in updateCell cellCostGrid w pathCost (x + 1, y) $
        updateCell cellCostGrid w pathCost (x - 1, y) $
          updateCell cellCostGrid w pathCost (x, y + 1) $
            updateCell cellCostGrid w pathCost (x, y - 1) $
              stateSetCell w (x, y) (Finished pathCost) $
                State grid (tail pendingCells) (finCount + 1)
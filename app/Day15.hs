module Day15 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 40, Case solveA "Problem" 0, Case solveB "Test" 315, Case solveB "Problem" 0]

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
      cellCostGrid = FastVector (Vector.fromList $ concat lines) []
      inputMap = InputMap cellCostGrid w h
      -- Set up state.
      initialPathCostGrid = FastVector (Vector.fromList (replicate (w * h) Never)) []
      emptyState = State initialPathCostGrid MinQueue.empty 0
      initialState = stateSetCell inputMap (0, 0) (Pending 0) emptyState
      -- Run until end cell is finished.
      isEndCellScoreFinished grid = isFinished $ gridGetCell grid w endCell
      State finalState _ _ = until (isEndCellScoreFinished . stateGrid) (finishNextCell inputMap) initialState
   in case gridGetCell finalState w endCell of
        Finished cost -> cost
        _ -> error "Cell not computed."

type Cost = Int

type CellCostGrid = FastVector Cost

type Width = Int

type Height = Int

type CellIdx = Int

type CellCoord = (Int, Int)

data InputMap = InputMap CellCostGrid Width Height

data Cell = Never | Pending Cost | Finished Cost deriving (Show)

isPending :: Cell -> Bool
isPending (Pending _) = True
isPending _ = False

isFinished :: Cell -> Bool
isFinished (Finished _) = True
isFinished _ = False

type PathCostGrid = FastVector Cell

type PendingCell = (Cost, CellIdx)

type PendingCells = MinQueue PendingCell

data State = State {stateGrid :: PathCostGrid, statePendingCells :: PendingCells, stateFinishedCount :: Int}

data FastVector a = FastVector (Vector a) [(Int, a)]

fvSet :: Int -> a -> FastVector a -> FastVector a
fvSet idx value (FastVector vec updates) =
  let updates' = (idx, value) : updates
   in if length updates > (round $ sqrt $ fromIntegral $ length vec)
        then FastVector (vec Vector.// updates') []
        else FastVector vec updates'

fvGet :: Int -> FastVector a -> a
fvGet idx (FastVector vec updates) = case find ((== idx) . fst) updates of
  Nothing -> vec Vector.! idx
  Just update -> snd update

idxToXY :: Width -> CellIdx -> CellCoord
idxToXY w idx = (idx `mod` w, idx `div` w)

pendingCellsAddCell :: PendingCell -> PendingCells -> PendingCells
pendingCellsAddCell = MinQueue.insert

gridGetCell :: FastVector a -> Width -> CellCoord -> a
gridGetCell grid w (x, y) = fvGet (y * w + x) grid

stateSetCell :: InputMap -> CellCoord -> Cell -> State -> State
stateSetCell (InputMap cellCostGrid w h) (x, y) newCellValue (State grid pendingCells finCount) =
  let cellIdx = y * w + x
      newPendingCells = case newCellValue of
        Pending cost -> pendingCellsAddCell (cost, cellIdx) pendingCells
        _ -> pendingCells
      newGrid = fvSet cellIdx newCellValue grid
   in State newGrid newPendingCells finCount

updateCell :: InputMap -> Cost -> CellCoord -> State -> State
updateCell (InputMap cellCostGrid w h) pathCost (x, y) (State grid pendingCells finCount)
  | x < 0 || y < 0 || x >= w || y >= h = state
  | otherwise =
    let c = (x, y)
        inputMap = (InputMap cellCostGrid w h)
        newPathCost = pathCost + gridGetCell cellCostGrid w c
     in case gridGetCell grid w c of
          Never -> stateSetCell inputMap c (Pending newPathCost) state
          Pending oldPathCost ->
            if newPathCost < oldPathCost
              then stateSetCell inputMap c (Pending newPathCost) state
              else state
          Finished _ -> state
  where
    state = State grid pendingCells finCount

finishNextCell :: InputMap -> State -> State
finishNextCell (InputMap cellCostGrid w h) (State grid pendingCells finCount) =
  let ((pathCost, pendingIdx), newPendingCells) = MinQueue.deleteFindMin pendingCells
      (x, y) = log $ idxToXY w pendingIdx
      inputMap = InputMap cellCostGrid w h
      log v = if finCount `mod` 10000 == 0 then trace (show (100 * finCount `div` (w * h)) ++ "%") v else v
   in updateCell inputMap pathCost (x + 1, y) $
        updateCell inputMap pathCost (x - 1, y) $
          updateCell inputMap pathCost (x, y + 1) $
            updateCell inputMap pathCost (x, y - 1) $
              stateSetCell inputMap (x, y) (Finished pathCost) $
                State grid newPendingCells (finCount + 1)
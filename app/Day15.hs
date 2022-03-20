module Day15 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (find, permutations, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 40, P solveB 2188189693529)

solveA :: [String] -> Int
solveA = solve

solveB :: [String] -> Int
solveB lines = 333

trace' :: Show a => a -> a
trace' value = trace (show value) value

solve :: [String] -> Int
solve lines =
  let w = length (head lines)
      h = length lines
      endCell = (w -1, h -1)
      cellCostGrid = map digitToInt $ concat lines
      emptyState = zip [0 ..] $ replicate (w * h) Never
      initialState = setCell w (0, 0) (Pending 0) emptyState
      finalState = until (\state -> isFinished $snd $getCell state w endCell) (finishNextCell cellCostGrid w) initialState
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

type State = [(Int, Cell)]

idxToXY w idx = (idx `mod` w, idx `div` w)

getCell :: [a] -> Int -> (Int, Int) -> a
getCell grid w (x, y) = grid !! (y * w + x)

setCell :: Int -> (Int, Int) -> Cell -> State -> State
setCell w (x, y) v grid =
  let i = y * w + x
   in take i grid ++ [(i, v)] ++ drop (i + 1) grid

updateCell :: [Int] -> Int -> Int -> (Int, Int) -> State -> State
updateCell cellCostGrid w pathCost (x, y) state
  | x < 0 || y < 0 || x >= w || y >= length state `div` w = state
  | otherwise =
    let c = (x, y)
        newPathCost = pathCost + getCell cellCostGrid w c
     in case snd $ getCell state w c of
          Never -> setCell w c (Pending newPathCost) state
          Pending oldPathCost ->
            if newPathCost < oldPathCost
              then setCell w c (Pending newPathCost) state
              else state
          Finished _ -> state

finishNextCell :: [Int] -> Int -> State -> State
finishNextCell cellCostGrid w state =
  let val = takePending . snd
      next = minimumBy (\x y -> compare (val x) (val y)) $ filter (isPending . snd) state
   in case next of
        (idx, Pending cost) ->
          let (x, y) = idxToXY w idx
           in updateCell cellCostGrid w cost (x + 1, y) $
                updateCell cellCostGrid w cost (x -1, y) $
                  updateCell cellCostGrid w cost (x, y + 1) $
                    updateCell cellCostGrid w cost (x, y -1) $
                      setCell w (x, y) (Finished cost) state
        _ -> error "Expected Pending value."
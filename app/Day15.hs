module Day15 where

import Data.Char (digitToInt)
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
      cellCostGrid = map digitToInt $ concat lines
      initialState = replicate (w * h) Nothing
      state = updateCell cellCostGrid w [((0, 0), 0)] (0, 0) initialState
   in case getCell state w (w -1, h -1) of
        Just cost -> cost - head cellCostGrid
        Nothing -> error "Cell not computed."

type State = [Maybe Int]

getCell :: [a] -> Int -> (Int, Int) -> a
getCell grid w (x, y) = grid !! (y * w + x)

setCell :: Int -> (Int, Int) -> Int -> State -> State
setCell w (x, y) v grid =
  let i = y * w + x
   in take i grid ++ [Just v] ++ drop (i + 1) grid

updateCell :: [Int] -> Int -> [((Int, Int), Int)] -> (Int, Int) -> State -> State
updateCell cellCostGrid w path (x, y) state
  | x < 0 || y < 0 || x >= w || y >= length state `div` w = state
  | otherwise =
    let c = (x, y)
        newPathCost = (snd . head) path + getCell cellCostGrid w c
        updateNeighbours =
          let updateCell' = updateCell cellCostGrid w ((c, newPathCost) : path)
           in updateCell' (x -1, y) $
                updateCell' (x, y -1) $
                  updateCell' (x + 1, y) $
                    updateCell' (x, y + 1) $
                      setCell w c newPathCost state
     in case getCell state w c of
          Nothing -> updateNeighbours
          Just oldPathCost ->
            if newPathCost < oldPathCost
              then
                if x == w -1 && y == (length state `div` w) -1
                  then trace ("better " ++ show newPathCost ++ "<" ++ show oldPathCost ++ ": " ++ show path) updateNeighbours
                  else updateNeighbours
              else state

--       let newState = setCell w c newPathCost state
--           updateCellFromHere = updateCell cellCostGrid w newPathCost
--           getNeighbourCall (x, y)
--             | x < 0 || y < 0 || x >= w || y >= length state `div` w = Nothing
--             | otherwise = Just (getCell cellCostGrid w (x, y), updateCellFromHere (x, y))
--           neighboursWithCost = mapMaybe getNeighbourCall [(x, y + 1), (x, y -1), (x + 1, y), (x -1, y)]
--           neighboursSortedByCost = map snd $ sortBy (\a b -> compare (fst a) (fst b)) neighboursWithCost
--           updateNeighbours = foldl1 (.) neighboursSortedByCost
--        in updateNeighbours newState
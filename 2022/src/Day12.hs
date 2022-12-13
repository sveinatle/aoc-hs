module Day12 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
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

cases =
  [ Case solveA "Test" 31,
    Problem solveA "Problem",
    Case solveB "Test" 29,
    Problem solveB "Problem"
  ]

data Map = Map {mCells :: Vector Cell, mWidth :: Int, mHeight :: Int} deriving (Show)

data Cell = Cell {cHeight :: Int, cDistance :: Int} deriving (Show)

xy2idx :: Int -> Int -> Map -> Int
xy2idx x y m = x + y * mWidth m

getCell :: Int -> Int -> Map -> Cell
getCell x y m = mCells m V.! xy2idx x y m

setCell :: Int -> Int -> Cell -> Map -> Map
setCell x y c m = Map (mCells m V.// [(xy2idx x y m, c)]) (mWidth m) (mHeight m)

char2int :: Char -> Int
char2int 'S' = ord 'a' - ord 'a'
char2int 'E' = ord 'z' - ord 'a'
char2int c = ord c - ord 'a'

readProblem :: [String] -> (Map, (Int, Int), (Int, Int))
readProblem lines =
  let width = length (head lines)
      height = length lines
      cells = V.fromList $ concat lines
      m = Map (V.map ((\h -> Cell h maxBound) . char2int) cells) width height
      idx2xy idx = (idx `mod` width, idx `div` width)
      start = case V.elemIndex 'S' cells of
        Just idx -> idx2xy idx
        Nothing -> error "Failed to find start point"
      end = case V.elemIndex 'E' cells of
        Just idx -> idx2xy idx
        Nothing -> error "Failed to find end point"
   in (m, start, end)

setDistance :: Int -> Int -> Int -> Map -> Map
setDistance x y d m =
  let c = getCell x y m
   in setCell x y (Cell (cHeight c) d) m

updateCell :: Int -> Int -> Int -> Int -> Map -> Map
updateCell x y fromHeight distance m =
  if x >= 0 && x < mWidth m && y >= 0 && y < mHeight m
    && fromHeight + 1 >= cHeight (getCell x y m)
    && cDistance (getCell x y m) > distance
    then (updateNeighbours x y . setDistance x y distance) m
    else m

updateNeighbours :: Int -> Int -> Map -> Map
updateNeighbours x y m =
  let distance = 1 + cDistance (getCell x y m)
      fromHeight = cHeight (getCell x y m)
   in updateCell (x + 1) y fromHeight distance $
        updateCell (x -1) y fromHeight distance $
          updateCell x (y + 1) fromHeight distance $
            updateCell x (y -1) fromHeight distance m

solveA :: [String] -> Int
solveA lines =
  let (m, (sx, sy), (ex, ey)) = readProblem lines
   in cDistance $ getCell ex ey $ updateCell sx sy 0 0 m

drawMap m = trace (show $ V.map cDistance (mCells m)) m

solveB :: [String] -> Int
solveB lines =
  let (m, (sx, sy), (ex, ey)) = readProblem lines
      finished = drawMap $ updateCell sx sy 0 0 m
      startCell = V.maximumBy (\a b -> compare (cDistance a) (cDistance b)) (V.filter ((== 0) . cHeight) (mCells finished))
   in cDistance (getCell ex ey finished) - cDistance startCell

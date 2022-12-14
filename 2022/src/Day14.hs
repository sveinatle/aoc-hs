{-# LANGUAGE TupleSections #-}

module Day14 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import Data.Ix
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.List.Split as V
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
  [ Case solveA "Test" 24,
    Problem solveA "Problem",
    Case solveB "Test" 93,
    Problem solveB "Problem"
  ]

data Map = Map {mCells :: Vector Cell, mWidth :: Int, mHeight :: Int, xRange :: (Int, Int), yRange :: (Int, Int)}

instance Show Map where
  show (Map cells width height (fromX, toX) (fromY, toY)) =
    show ((fromX, toX), (fromY, toY)) ++ "\n"
      ++ ( intercalate "\n"
             . map (take (1 + toX - fromX) . drop fromX)
             . take (1 + toY - fromY)
             . drop fromY
             . chunksOf width
             . V.toList
             . V.map (head . show)
         )
        cells

data Cell = Air | Rock | Sand

instance Show Cell where
  show Air = "."
  show Rock = "#"
  show Sand = "o"

xy2idx :: Int -> Int -> Map -> Int
xy2idx x y m = x + y * mWidth m

createMap :: Int -> Int -> Map
createMap w h = Map (V.replicate (w * h) Air) w h (maxBound, minBound) (maxBound, minBound)

getCell :: Int -> Int -> Map -> Cell
getCell x y m = mCells m V.! xy2idx x y m

setCell :: Int -> Int -> Cell -> Map -> Map
setCell x y c m =
  let updateRange v (from, to) = (min v from, max v to)
   in Map (mCells m V.// [(xy2idx x y m, c)]) (mWidth m) (mHeight m) (updateRange x (xRange m)) (updateRange y (yRange m))

loadProblem :: [String] -> Map
loadProblem = setCell 500 0 Air . foldl loadPath (createMap 800 600) -- Set cell (500,0) to extends bounds up to insertion position.

loadPath :: Map -> String -> Map
loadPath m line =
  let readPoint = (\[x, y] -> (x, y)) . map read . splitOn ","
      points = (map readPoint . splitOn " -> ") line
      segments = zip points (tail points)
   in foldl loadSegment m segments

loadSegment :: Map -> ((Int, Int), (Int, Int)) -> Map
loadSegment m ((x1, y1), (x2, y2)) =
  let range v1 v2 = [(min v1 v2) .. (max v1 v2)]
      coords = [(x, y) | x <- range x1 x2, y <- range y1 y2]
   in foldl loadCoords m coords

loadCoords :: Map -> (Int, Int) -> Map
loadCoords m (x, y) = setCell x y Rock m

simulateSand :: Map -> (Int, Int) -> Maybe (Int, Int)
simulateSand m (x, y) =
  if not (inRange (xRange m) x && inRange (yRange m) y)
    then Nothing
    else case (getCell (x -1) (y + 1) m, getCell x (y + 1) m, getCell (x + 1) (y + 1) m) of
      (_, Air, _) -> simulateSand m (x, y + 1)
      (Air, _, _) -> simulateSand m (x -1, y + 1)
      (_, _, Air) -> simulateSand m (x + 1, y + 1)
      _ -> Just (x, y)

data SimState = SimState {sDone :: Bool, sCount :: Int, sMap :: Map} deriving (Show)

addSand :: SimState -> SimState
addSand (SimState done count m) = case simulateSand m (500, 0) of
  Just (500, 0) -> SimState True (count + 1) (setCell 500 0 Sand m)
  Just (x, y) -> SimState False (count + 1) (setCell x y Sand m)
  Nothing -> SimState True count m

drawAndGetSandCount :: SimState -> Int
drawAndGetSandCount (SimState done count m) = trace (show m) count

solveA :: [String] -> Int
solveA = drawAndGetSandCount . until sDone addSand . SimState False 0 . loadProblem

solveB :: [String] -> Int
solveB =
  let addGround m =
        let groundY = ((+ 2) . snd . yRange) m
         in loadSegment m ((300, groundY), (700, groundY))
   in sCount . until sDone addSand . SimState False 0 . addGround . loadProblem
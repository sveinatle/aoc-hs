{-# LANGUAGE TupleSections #-}

module Day22 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import qualified Data.Foldable as HashSet
import qualified Data.HashSet as HashSet
import Data.Ix
import Data.List as List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.List.Split as V
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
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
  [ Case solveA "Test" 6032,
    Problem solveA "Problem",
    Case solveB "Test" 5031,
    Problem solveB "Problem"
  ]

data Move = Fwd Int | TurnLeft | TurnRight deriving (Show)

data Direction = N | E | S | W deriving (Show, Eq)

getDirection :: Direction -> Bool -> Direction
getDirection dir clockwise =
  let newIdx = fromJust (elemIndex dir directions) + (if clockwise then 1 else -1)
   in directions !! (newIdx `mod` length directions)

directions = [E, S, W, N]

data Position = Position {pX :: Int, pY :: Int, pD :: Direction} deriving (Show)

type Map = Vector (Vector Char)

readMap :: [String] -> Map
readMap lines =
  let width = maximum (map length lines)
   in V.fromList (map (\line -> V.fromList (take width $ line ++ repeat ' ')) lines)

readMoves :: [Char] -> [Move] -> [Move]
readMoves [] moves = moves
readMoves ('L' : rest) moves = readMoves rest (TurnLeft : moves)
readMoves ('R' : rest) moves = readMoves rest (TurnRight : moves)
readMoves str moves =
  let numStr = takeWhile isDigit str
      rest = drop (length numStr) str
   in readMoves rest (Fwd (read numStr) : moves)

applyMove :: Map -> Position -> Move -> Position
applyMove m (Position x y dir) TurnLeft = Position x y (getDirection dir False)
applyMove m (Position x y dir) TurnRight = Position x y (getDirection dir True)
applyMove m (Position x y dir) (Fwd count) = moveForward m (Position x y dir) count

moveForward :: Map -> Position -> Int -> Position
moveForward m (Position x y dir) count =
  if count == 0
    then Position x y dir
    else
      let (nextX, nextY) = getNextCoord m x y dir
       in if (nextX, nextY) == (x, y) -- If was blocked.
            then Position nextX nextY dir -- Then stop moving.
            else moveForward m (Position nextX nextY dir) (count - 1) -- Else keep moving.

getNextCoord :: Map -> Int -> Int -> Direction -> (Int, Int)
getNextCoord m x y dir =
  let (nextX, nextY) = case dir of
        N -> normalize m (x, y -1)
        E -> normalize m (x + 1, y)
        S -> normalize m (x, y + 1)
        W -> normalize m (x -1, y)
   in case getXY m nextX nextY of
        '.' -> (nextX, nextY)
        '#' -> (x, y)
        ' ' -> getNextAfterWrap m x y dir
        _ -> error "Unexpected map character."

normalize :: Map -> (Int, Int) -> (Int, Int)
normalize m (x, y) =
  let height = V.length m
      width = V.length (V.head m)
   in ((width + x) `mod` width, (height + y) `mod` height)

getNextAfterWrap :: Map -> Int -> Int -> Direction -> (Int, Int)
getNextAfterWrap m x y dir =
  let row = m V.! y
      col = V.map (V.! x) m
      findFirstIndex test vec = fromJust $ V.findIndex test vec
      findLastIndex test vec = (V.length vec - 1) - fromJust (V.findIndex test (V.reverse vec))
      (nextX, nextY) = case dir of
        N -> (x, findLastIndex (' ' /=) col)
        E -> (findFirstIndex (' ' /=) row, y)
        S -> (x, findFirstIndex (' ' /=) col)
        W -> (findLastIndex (' ' /=) row, y)
   in case getXY m nextX nextY of
        '.' -> (nextX, nextY)
        '#' -> (x, y)
        _ -> error "Unexpected map character."

getXY :: Map -> Int -> Int -> Char
getXY m x y = (m V.! y) V.! x

computePassword :: Position -> Int
computePassword (Position x y dir) = 1000 * (y + 1) + 4 * (x + 1) + fromJust (elemIndex dir directions)

solveA :: [String] -> Int
solveA lines =
  let [mapStr, [movesStr]] = splitOn [""] lines
      m = readMap mapStr
      moves = reverse $ readMoves movesStr []
      x = case V.elemIndex '.' (m V.! 0) of
        Just x -> x
        Nothing -> error "Failed to get initial X position."
      initialPosition = Position x 0 E
   in computePassword $ foldl (applyMove m) initialPosition moves

solveB :: [String] -> Int
solveB lines = 0

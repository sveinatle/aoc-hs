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
import Data.Map (Map)
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
    Case solveBTest "Test" 5031,
    Problem solveBProblem "Problem"
  ]

data Move = Fwd Int | TurnLeft | TurnRight deriving (Show)

data Direction = N | E | S | W deriving (Show, Eq, Ord)

getDirection :: Direction -> Bool -> Direction
getDirection dir clockwise =
  let newIdx = fromJust (elemIndex dir directions) + (if clockwise then 1 else -1)
   in directions !! (newIdx `mod` length directions)

directions = [E, S, W, N]

data Position = Position {pX :: Int, pY :: Int, pD :: Direction} deriving (Show, Eq, Ord)

data World = World {wTiles :: Vector (Vector Char), wWraps :: Map Position Position}

isInside :: World -> Int -> Int -> Bool
isInside (World tiles wraps) x y =
  let height = V.length tiles
      width = V.length (V.head tiles)
   in x >= 0 && x < width && y >= 0 && y < height

getXY :: World -> Int -> Int -> Char
getXY (World tiles wraps) x y = (tiles V.! y) V.! x

findFirstIndex test vec = fromJust $ V.findIndex test vec

findLastIndex test vec = (V.length vec - 1) - fromJust (V.findIndex test (V.reverse vec))

readMap :: [String] -> Vector (Vector Char)
readMap lines =
  let width = maximum (map length lines)
   in V.fromList (map (\line -> V.fromList (take width $ line ++ repeat ' ')) lines)

readSimpleWraps :: Vector (Vector Char) -> Map Position Position
readSimpleWraps tiles =
  let dirAndCoord2Pos dir (x, y) = Position x y dir
      westEdges = zip (V.toList $ V.map (findFirstIndex (/= ' ')) tiles) [0 ..]
      eastEdges = zip (V.toList $ V.map (findLastIndex (/= ' ')) tiles) [0 ..]
      west2east = zip (map (dirAndCoord2Pos W) westEdges) (map (dirAndCoord2Pos W) eastEdges)
      east2west = zip (map (dirAndCoord2Pos E) eastEdges) (map (dirAndCoord2Pos E) westEdges)
      getColumn x = V.map (V.! x) tiles
      width = V.length (V.head tiles)
      northEdges = map (\x -> (x, findFirstIndex (/= ' ') (getColumn x))) [0 .. (width -1)]
      southEdges = map (\x -> (x, findLastIndex (/= ' ') (getColumn x))) [0 .. (width -1)]
      north2south = zip (map (dirAndCoord2Pos N) northEdges) (map (dirAndCoord2Pos N) southEdges)
      south2north = zip (map (dirAndCoord2Pos S) southEdges) (map (dirAndCoord2Pos S) northEdges)
   in Map.fromList $ concat [west2east, east2west, north2south, south2north]

side :: (Int, Int) -> (Int, Int) -> Direction -> (Int, Int) -> (Int, Int) -> Direction -> [(Position, Position)]
side (sourceFromX, sourceFromY) (sourceToX, sourceToY) sourceDir (targetFromX, targetFromY) (targetToX, targetToY) targetDir =
  let r a b = if b >= a then [a .. b] else reverse [b .. a]
      sourcePositions = [Position x y sourceDir | x <- r sourceFromX sourceToX, y <- r sourceFromY sourceToY]
      targetCoords = [Position x y targetDir | x <- r targetFromX targetToX, y <- r targetFromY targetToY]
   in zip sourcePositions targetCoords

cubeWrapsTest :: Map Position Position
cubeWrapsTest =
  let s = 4
   in Map.fromList $
        concat -- Clockwise from top-left.
          [ side (8, 0) (11, 0) N (3, 4) (0, 4) S,
            side (11, 0) (11, 3) E (15, 11) (15, 8) W,
            side (11, 4) (11, 7) E (15, 8) (12, 8) S,
            side (12, 8) (15, 8) N (11, 7) (11, 4) W,
            side (15, 8) (15, 11) E (11, 3) (11, 0) W,
            side (15, 11) (12, 11) S (0, 4) (0, 7) E,
            side (11, 11) (8, 11) S (0, 7) (3, 7) N,
            side (8, 11) (8, 8) W (4, 7) (7, 7) N,
            side (7, 7) (4, 7) S (8, 8) (8, 11) E,
            side (3, 7) (0, 7) S (8, 11) (11, 11) N,
            side (0, 7) (0, 4) W (12, 11) (15, 11) N,
            side (0, 4) (3, 4) N (11, 0) (9, 0) S,
            side (4, 4) (7, 4) N (8, 0) (8, 3) E,
            side (8, 3) (8, 0) W (7, 4) (4, 4) S
          ]

cubeWrapsProblem :: Map Position Position
cubeWrapsProblem =
  Map.fromList $
    concat -- Clockwise from top-left.
      [ side (50, 0) (99, 0) N (0, 150) (0, 199) E, -- 0
        side (100, 0) (149, 0) N (0, 199) (49, 199) N, -- 1
        side (149, 0) (149, 49) E (99, 149) (99, 100) W, -- 2
        side (149, 49) (100, 49) S (99, 99) (99, 50) W, -- 3
        side (99, 99) (99, 50) E (149, 49) (100, 49) N, -- 4
        side (99, 149) (99, 100) E (149, 0) (149, 49) W, -- 5
        side (99, 149) (50, 149) S (49, 199) (49, 150) W, -- 6
        side (49, 199) (49, 150) E (99, 149) (50, 149) N, -- 7
        side (0, 199) (49, 199) S (100, 0) (149, 0) S, -- 8
        side (0, 150) (0, 199) W (50, 0) (99, 0) S, -- 9
        side (0, 149) (0, 100) W (50, 0) (50, 49) E, -- 10
        side (0, 100) (49, 100) N (50, 50) (50, 99) E, -- 11
        side (50, 50) (50, 99) W (0, 100) (49, 100) S, -- 12
        side (50, 0) (50, 49) W (0, 149) (0, 100) E -- 13
      ]

readMoves :: [Char] -> [Move] -> [Move]
readMoves [] moves = moves
readMoves ('L' : rest) moves = readMoves rest (TurnLeft : moves)
readMoves ('R' : rest) moves = readMoves rest (TurnRight : moves)
readMoves str moves =
  let numStr = takeWhile isDigit str
      rest = drop (length numStr) str
   in readMoves rest (Fwd (read numStr) : moves)

applyMove :: World -> Position -> Move -> Position
applyMove world (Position x y dir) TurnLeft = Position x y (getDirection dir False)
applyMove world (Position x y dir) TurnRight = Position x y (getDirection dir True)
applyMove world (Position x y dir) (Fwd count) = moveForward world (Position x y dir) count

moveForward :: World -> Position -> Int -> Position
moveForward world (Position x y dir) count =
  if count == 0
    then Position x y dir
    else
      let (Position nextX nextY nextDir) = getNextPosition world x y dir
       in if (nextX, nextY) == (x, y) -- If was blocked.
            then Position nextX nextY dir -- Then stop moving.
            else moveForward world (Position nextX nextY nextDir) (count - 1) -- Else keep moving.

getNextPosition :: World -> Int -> Int -> Direction -> Position
getNextPosition world x y dir =
  let (nextX, nextY) = case dir of
        N -> (x, y -1)
        E -> (x + 1, y)
        S -> (x, y + 1)
        W -> (x -1, y)
   in if isInside world nextX nextY
        then case getXY world nextX nextY of
          '.' -> Position nextX nextY dir
          '#' -> Position x y dir
          ' ' -> getNextAfterWrap world x y dir
          _ -> error "Unexpected map character."
        else getNextAfterWrap world x y dir

getNextAfterWrap :: World -> Int -> Int -> Direction -> Position
getNextAfterWrap world x y dir =
  let tiles = wTiles world
      wraps = wWraps world
      row = tiles V.! y
      col = V.map (V.! x) tiles
      (Position nextX nextY nextDir) = case wraps Map.!? Position x y dir of
        Just p -> p
        Nothing -> error $ "Wrap not found " ++ show (x, y, dir) ++ "."
   in case getXY world nextX nextY of
        '.' -> Position nextX nextY nextDir
        '#' -> Position x y dir
        _ -> error "Unexpected map character."

computePassword :: Position -> Int
computePassword (Position x y dir) = 1000 * (y + 1) + 4 * (x + 1) + fromJust (elemIndex dir directions)

solveA :: [String] -> Int
solveA lines =
  let [mapStr, [movesStr]] = splitOn [""] lines
      tiles = readMap mapStr
      world = World tiles (readSimpleWraps tiles)
      moves = reverse $ readMoves movesStr []
      x = case V.elemIndex '.' (tiles V.! 0) of
        Just x -> x
        Nothing -> error "Failed to get initial X position."
      initialPosition = Position x 0 E
   in computePassword $ foldl (applyMove world) initialPosition moves

solveB :: [String] -> Map Position Position -> Int
solveB lines cubeWraps =
  let [mapStr, [movesStr]] = splitOn [""] lines
      tiles = readMap mapStr
      world = World tiles cubeWraps
      moves = reverse $ readMoves movesStr []
      x = case V.elemIndex '.' (tiles V.! 0) of
        Just x -> x
        Nothing -> error "Failed to get initial X position."
      initialPosition = Position x 0 E
   in computePassword $ foldl (applyMove world) initialPosition moves

solveBTest :: [String] -> Int
solveBTest lines = solveB lines cubeWrapsTest

solveBProblem :: [String] -> Int
solveBProblem lines = solveB lines cubeWrapsProblem
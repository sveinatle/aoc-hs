{-# LANGUAGE TupleSections #-}

module Day24 where

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
import Data.Set (Set)
import qualified Data.Set as Set
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
  [ Case solveA "Test" 18,
    Problem solveA "Problem",
    Case solveB "Test" 54,
    Problem solveB "Problem"
  ]

type Blizzard = ((Int, Int), (Int, Int))

data State = State {sBlizzards :: [Blizzard], sPositions :: [(Int, Int)], sStepCount :: Int} deriving (Show)

readBlizzards :: [String] -> (Int, Int, [Blizzard])
readBlizzards lines =
  let width = length (head lines) - 2
      height = length lines - 2
      cells = concatMap (tail . init) (tail $ init lines)
      char2dir c = case c of
        '<' -> (-1, 0)
        '>' -> (1, 0)
        '^' -> (0, -1)
        'v' -> (0, 1)
        _ -> error "Unexpected character."
      idx2coord i = (i `mod` width, i `div` width)
      blizzards = map (\(idx, dirChar) -> (idx2coord idx, char2dir dirChar)) $ filter (('.' /=) . snd) $ zip [0 ..] cells
   in (width, height, blizzards)

stepBlizzard :: Int -> Int -> Blizzard -> Blizzard
stepBlizzard w h ((x, y), (dx, dy)) = (((x + dx + w) `mod` w, (y + dy + h) `mod` h), (dx, dy))

stepBlizzards :: Int -> Int -> [Blizzard] -> [Blizzard]
stepBlizzards w h blizzards = map (stepBlizzard w h) blizzards

entryPosition :: (Int, Int)
entryPosition = (0, -1)

exitPosition :: Int -> Int -> (Int, Int)
exitPosition w h = (w -1, h)

getNextPositions :: Int -> Int -> [Blizzard] -> (Int, Int) -> [(Int, Int)]
getNextPositions w h nextBlizzards (x, y) =
  let isPositionBlizzard x y = case find (\(pos, speed) -> pos == (x, y)) nextBlizzards of
        Just _ -> True
        Nothing -> False
      isPositionOk x y =
        (x, y) == exitPosition w h
          || (x, y) == entryPosition
          || x >= 0 && x < w && y >= 0 && y < h && not (isPositionBlizzard x y)
   in filter (uncurry isPositionOk) [(x, y), (x -1, y), (x, y -1), (x + 1, y), (x, y + 1)]

step :: Int -> Int -> State -> State
step w h (State blizzards positions stepCount) =
  let nextBlizzards = stepBlizzards w h blizzards
      nextPositions = case nub $ sort $ concatMap (getNextPositions w h nextBlizzards) positions of
        [] -> error "No positions left."
        ps -> ps
   in State nextBlizzards nextPositions (stepCount + 1)

solveA :: [String] -> Int
solveA lines =
  let (w, h, blizzards) = readBlizzards lines
      hasExitPosition = elem (exitPosition w h) . sPositions
   in sStepCount $ until hasExitPosition (step w h) (State blizzards [entryPosition] 0)

solveB :: [String] -> Int
solveB lines =
  let (w, h, blizzards) = readBlizzards lines
      hasExitPosition = elem (exitPosition w h) . sPositions
      hasEntryPosition = elem entryPosition . sPositions
      one = until hasExitPosition (step w h) (State blizzards [entryPosition] 0)
      two = until hasEntryPosition (step w h) (State (sBlizzards one) [exitPosition w h] 0)
      three = until hasExitPosition (step w h) (State (sBlizzards two) [entryPosition] 0)
   in sum $ map sStepCount [one, two, three]
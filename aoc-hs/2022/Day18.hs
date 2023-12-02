{-# LANGUAGE TupleSections #-}

module Day18 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import qualified Data.Foldable as HashSet
import qualified Data.HashSet as HashSet
import Data.Ix
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import qualified Data.List.Split as V
import Data.Maybe (isJust)
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
  [ Case solveA "Test" 64,
    Problem solveA "Problem",
    Case solveB "Test" 58,
    Problem solveB "Problem"
  ]

solveA :: [String] -> Int
solveA lines =
  let cubes = map readCube lines
   in sum $ map ((6 -) . countNeighbours (HashSet.fromList cubes)) cubes

getNeighbours (x, y, z) =
  [ (x -1, y, z),
    (x + 1, y, z),
    (x, y -1, z),
    (x, y + 1, z),
    (x, y, z -1),
    (x, y, z + 1)
  ]

countNeighbours :: HashSet.HashSet (Int, Int, Int) -> (Int, Int, Int) -> Int
countNeighbours cubes coord =
  length $
    filter (True ==) $
      map
        (`HashSet.elem` cubes)
        (getNeighbours coord)

readCube :: String -> (Int, Int, Int)
readCube line = case splitOn "," line of
  [x, y, z] -> (read x, read y, read z)
  _ -> error "Failed to read cube."

solveB :: [String] -> Int
solveB lines =
  let cubes = map readCube lines
      cubesHashSet = HashSet.fromList cubes
      outerCubesHashSet = flood cubesHashSet (HashSet.fromList []) (0, 0, 0)
   in sum $ map (countNeighbours outerCubesHashSet) cubes

flood :: HashSet.HashSet (Int, Int, Int) -> HashSet.HashSet (Int, Int, Int) -> (Int, Int, Int) -> HashSet.HashSet (Int, Int, Int)
flood cubes spaces coord =
  let isOutOfBounds (x, y, z) = x < -1 || x > 22 || y < -1 || y > 22 || z < -1 || z > 22
      f spaces neighbour =
        if isOutOfBounds neighbour || neighbour `HashSet.elem` spaces || neighbour `HashSet.elem` cubes
          then spaces
          else flood cubes (HashSet.insert neighbour spaces) neighbour
   in foldl f spaces (getNeighbours coord)

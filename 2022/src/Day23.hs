{-# LANGUAGE TupleSections #-}

module Day23 where

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
  [ Case solveA "Test" 110,
    Problem solveA "Problem",
    Case solveB "Test" 20,
    Problem solveB "Problem"
  ]

readPositions :: [String] -> [(Int, Int)]
readPositions lines =
  let width = length (head lines)
      height = length lines
   in map (\(i, _) -> (i `mod` width, i `div` width)) . filter (('#' ==) . snd) $ zip [0 ..] (concat lines)

drawPositions :: [(Int, Int)] -> [(Int, Int)]
drawPositions positions =
  let minX = minimum $ map fst positions
      maxX = maximum $ map fst positions
      minY = minimum $ map snd positions
      maxY = maximum $ map snd positions
      width = maxX + 1 - minX
      height = maxY + 1 - minY
      positionsIndices = map (\(x, y) -> (y - minY) * width + (x - minX)) positions
      positionsStr =
        intercalate "\n" $
          chunksOf width $
            V.toList $
              V.generate (width * height) (\idx -> if idx `elem` positionsIndices then '#' else '.')
   in trace ('\n' : positionsStr) positions

process :: (Bool, [(Int, Int)], Int) -> (Bool, [(Int, Int)], Int)
process (done, positions, iteration) =
  let positionsSet = Set.fromList positions
      (satisfied, proposed) = unzip $ map (proposePosition positionsSet iteration) positions
      acceptedSet = Set.fromList $ map head $ filter ((==) 1 . length) $ group $ sort proposed
      positionsWithProposed = zip positions proposed
      pickNext pos prop = if Set.member prop acceptedSet then prop else pos
      allSatisifies = and satisfied
      nextPositions = map (uncurry pickNext) positionsWithProposed
   in (allSatisifies {-drawPositions-}, nextPositions, iteration + 1)

proposePosition :: Set (Int, Int) -> Int -> (Int, Int) -> (Bool, (Int, Int))
proposePosition positions iteration (x, y) =
  let isOccupied x y = Set.member (x, y) positions
      n = isOccupied (x + 0) (y - 1)
      ne = isOccupied (x + 1) (y - 1)
      e = isOccupied (x + 1) (y + 0)
      se = isOccupied (x + 1) (y + 1)
      s = isOccupied (x + 0) (y + 1)
      sw = isOccupied (x - 1) (y + 1)
      w = isOccupied (x - 1) (y + 0)
      nw = isOccupied (x - 1) (y - 1)
      stay = True `notElem` [n, ne, e, se, s, sw, w, nw]
      firstAvailableDirection =
        find ((==) False . fst) $
          take 4 $
            drop iteration $
              cycle
                [ (nw || n || ne, (x, y - 1)),
                  (sw || s || se, (x, y + 1)),
                  (nw || w || sw, (x - 1, y)),
                  (ne || e || se, (x + 1, y))
                ]
   in if stay
        then (True, (x, y))
        else
          ( False,
            case firstAvailableDirection of
              Just (directionIsFree, propPos) -> propPos
              Nothing -> (x, y) -- Nowhere to go.
          )

countFreeTiles :: [(Int, Int)] -> Int
countFreeTiles positions =
  let minX = minimum $ map fst positions
      maxX = maximum $ map fst positions
      minY = minimum $ map snd positions
      maxY = maximum $ map snd positions
      elfCount = length positions
   in (maxX + 1 - minX) * (maxY + 1 - minY) - elfCount

getPositionsFromState :: (a, b, c) -> b
getPositionsFromState (_, positions, _) = positions

getDoneFromState :: (a, b, c) -> a
getDoneFromState (done, _, _) = done

getIterationsFromState :: (a, b, c) -> c
getIterationsFromState (_, _, iterations) = iterations

solveA :: [String] -> Int
solveA lines =
  let initialPositions = readPositions lines
      initialState = (False, initialPositions, 0)
   in countFreeTiles $ getPositionsFromState $ iterate process initialState !! 10

solveB :: [String] -> Int
solveB lines =
  let initialPositions = readPositions lines
      initialState = (False, initialPositions, 0)
   in getIterationsFromState $ until getDoneFromState process initialState
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Day09 where

import Data.Char
import Data.Foldable (Foldable (foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import DayProblem (Case (Case, Problem))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

t0 v = trace (show v) 0

t' txt v = trace (txt ++ show v) v

cases =
  [ Case solveA "Test" 13,
    Problem solveA "Problem",
    Case solveB "Test" 0,
    Problem solveB "Problem"
  ]

type Pos = (Int, Int)

type Movement = (Int, Int)

type State = (Pos, Pos)

type History = [State]

solveA :: [String] -> Int
solveA lines = length $ nub $ sort $ map snd $ foldl processLine [((0, 0), (0, 0))] lines

processLine :: History -> String -> History
processLine history (dirChar : ' ' : count) = iterate (move (c2m dirChar)) history !! read count
processLine _ line = error $ "Unexpected input: " ++ line

c2m 'U' = (0, 1)
c2m 'D' = (0, -1)
c2m 'R' = (1, 0)
c2m 'L' = (-1, 0)
c2m _ = error "Unexpected direction."

-- h = head
-- t = tail
-- m = movement
-- d = distance
move :: Movement -> History -> History
move (mx, my) history =
  let ((hx, hy), (tx, ty)) = head history
      h2 = (hx + mx, hy + my)
      (hx2, hy2) = h2
      (dx, dy) = (tx - hx2, ty - hy2)
      t2 =
        if abs dx <= 1 && abs dy <= 1
          then (tx, ty) -- Stay put. Only need to move tail when it is too far away.
          else
            ( hx2 + (dx `quot` 2), -- Distance of 2 will become 1 to catch up. 1 will become 0, aligning it with movement direction of the head.
              hy2 + (dy `quot` 2) -- (2,2) will become (1,1), letting it catch up diagonally.
            )
   in (h2, t2) : history

solveB :: [String] -> Int
solveB lines = 0
{-# LANGUAGE TupleSections #-}

module Day16 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
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
  [ Case solveA "Test" 1651,
    Problem solveA "Problem",
    Case solveB "Test" 99,
    Problem solveB "Problem"
  ]

readValve :: String -> (String, Int, [String])
readValve line = case matchRegex (mkRegex "Valve (.*) has flow rate=(.*);.*valves? (.*)") line of
  Just [valve, rateStr, tunnelsStr] -> (valve, read rateStr, splitOn ", " tunnelsStr)
  Just _ -> error "Unexpected match result."
  Nothing -> error "Failed to read line."

solveA :: [String] -> Int
solveA lines =
  let valves = map readValve lines
   in t0 valves

solveB :: [String] -> Int
solveB lines = 0

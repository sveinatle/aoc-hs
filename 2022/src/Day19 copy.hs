{-# LANGUAGE TupleSections #-}

module Day19 where

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
  [ Case solveA "Test" 0,
    Problem solveA "Problem",
    Case solveB "Test" 0,
    Problem solveB "Problem"
  ]

type Resources = [Int]

type RobotCost = Resources

type Blueprint = (Int, [RobotCost])

data State = State {sBp :: Blueprint, sMinute :: Int, sResources :: Resources, sRobots :: [Int]} deriving (Show)

rGeodes (_, _, _, g) = g

minutes = 10

addBot :: Int -> [Int] -> [Int]
addBot idx bots = take idx bots ++ ((bots !! idx) + 1) : drop (idx + 1) bots

readBlueprint :: String -> Blueprint
readBlueprint line = case matchRegex (mkRegex "Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.") line of
  Just [bp, aOre, bOre, cOre, cClay, dOre, dObsidian] -> (read bp, [[read aOre, 0, 0, 0], [read bOre, 0, 0, 0], [read cOre, read cClay, 0, 0], [read dOre, 0, read dObsidian, 0]])
  _ -> error "Parse error."

evaluateBlueprint :: Blueprint -> State
evaluateBlueprint bp =
  let initialState = State bp 0 [0, 0, 0, 0] [1, 0, 0, 0]
      (doneStates, _) = until (null . snd) explore ([], [initialState])
   in maximumBy (\a b -> compare (last $ sResources a) (last $ sResources b)) doneStates

explore :: ([State], [State]) -> ([State], [State])
explore (done, inprogress) =
  let newStates = concatMap getStatesNextMinute inprogress
      done' = filter ((==) minutes . sMinute) newStates
      inprogress' = filter (not . (==) minutes . sMinute) newStates
   in (done' ++ done, inprogress' ++ inprogress)

getStatesNextMinute :: State -> [State]
getStatesNextMinute state =
  let (State bp minute resources existingBots) = state
   in if sMinute state >= minutes
        then error "Unexpected minute"
        else
          let newStatesThisMinute = state : getStatesWithNewBotsThisMinute state
              newStatesNextMinute = map (\(State bp _ resources newBots) -> State bp (minute + 1) (zipWith (+) resources existingBots) newBots) newStatesThisMinute
           in newStatesNextMinute

getStatesWithNewBotsThisMinute :: State -> [State]
getStatesWithNewBotsThisMinute (State (bpId, costs) minute resources bots) =
  let remainingResources = map (zipWith (-) resources) costs
      affordedBotsWithNewResources = filter (all (>= 0) . snd) $ zip [0 ..] remainingResources
      newStates = map (\(botIdx, newResources) -> State (bpId, costs) minute newResources (addBot botIdx bots)) affordedBotsWithNewResources
   in concatMap getStatesWithNewBotsThisMinute newStates ++ newStates -- Recurse in case we can get more than one new bot in the same minute.

solveA :: [String] -> Int
solveA lines =
  let bestStatePerBlueprint = map (evaluateBlueprint . readBlueprint) lines
   in length $ t bestStatePerBlueprint

solveB :: [String] -> Int
solveB lines = 999

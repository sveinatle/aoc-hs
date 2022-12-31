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
  [ Case solveA "Test" 33,
    Problem solveA "Problem",
    Case solveB "Test" 0,
    Problem solveB "Problem"
  ]

type Resources = [Int]

type RobotCost = Resources

type Blueprint = (Int, [RobotCost])

rGeodes (_, _, _, g) = g

minuteCount = 24

addBot :: Int -> [Int] -> [Int]
addBot idx bots = take idx bots ++ ((bots !! idx) + 1) : drop (idx + 1) bots

readBlueprint :: String -> Blueprint
readBlueprint line = case matchRegex (mkRegex "Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.") line of
  Just [bp, aOre, bOre, cOre, cClay, dOre, dObsidian] -> (read bp, [[read aOre, 0, 0, 0], [read bOre, 0, 0, 0], [read cOre, read cClay, 0, 0], [read dOre, 0, read dObsidian, 0]])
  _ -> error "Parse error."

getNeededDays :: Int -> Int -> Int
getNeededDays neededResource botCount = case (neededResource == 0, botCount == 0) of
  (False, True) -> 99999
  (True, True) -> 0
  _ -> (neededResource + botCount - 1) `div` botCount -- Divide rounding up.

tryEachBot :: Blueprint -> [Int] -> Int -> [Int] -> [Int] -> [Int] -> Int
tryEachBot (bpId, costs) botList minute bots addedBots resources =
  maximum $ map (handleBot (bpId, costs) botList minute bots addedBots resources) [3, 2, 1, 0]

--if (sum bots > 4 && bots !! 2 == 0) || (sum bots > 7 && bots !! 3 == 0)
--  then 0
--  else

handleBot :: Blueprint -> [Int] -> Int -> [Int] -> [Int] -> [Int] -> Int -> Int
handleBot (bpId, costs) botList minute bots addedBotsThisMinute resources botIdx =
  let botCost = costs !! botIdx
      remainingResources = zipWith (-) resources botCost
      canCreateBotThisMinute = all (>= 0) remainingResources
   in if canCreateBotThisMinute
        then -- Can create bot now. Try to create another bot too.
          tryEachBot (bpId, costs) (botIdx : botList) minute bots (addBot botIdx addedBotsThisMinute) remainingResources
        else -- Move to minute when the bot can be created.

          let resourcesAfterThisMinute = zipWith (+) resources bots
              neededResourcesAfterThisMinute = zipWith (\c r -> max 0 (c - r)) botCost resourcesAfterThisMinute
              botsAfterThisMinute = zipWith (+) bots addedBotsThisMinute
              neededMinutesAfterThisMinute = maximum $ zipWith getNeededDays neededResourcesAfterThisMinute botsAfterThisMinute
              createBotMinute = minute + 1 + neededMinutesAfterThisMinute
           in if createBotMinute < minuteCount
                then
                  let resourcesBeforeCreateBot = zipWith (\r b -> r + b * neededMinutesAfterThisMinute) resourcesAfterThisMinute botsAfterThisMinute
                   in handleBot (bpId, costs) botList createBotMinute botsAfterThisMinute [0, 0, 0, 0] resourcesBeforeCreateBot botIdx
                else -- Selected bot will not be created. Finish by computing final obsidian count.

                  let geodes = last resourcesAfterThisMinute + last botsAfterThisMinute * (minuteCount - minute)
                   in if geodes > 6
                        then trace (show bpId ++ " + " ++ show geodes ++ ": " ++ show (reverse botList)) geodes
                        else geodes

evaluateBlueprint :: Blueprint -> Int
evaluateBlueprint (bpId, costs) = tryEachBot (bpId, costs) [] 1 [1, 0, 0, 0] [0, 0, 0, 0] [0, 0, 0, 0]

solveA :: [String] -> Int
solveA lines =
  let maxGeodesPerBlueprint = t $ map (evaluateBlueprint . readBlueprint) lines
   in sum $ zipWith (*) maxGeodesPerBlueprint [1 ..]

solveB :: [String] -> Int
solveB lines = 999

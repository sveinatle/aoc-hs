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
import qualified Data.Maybe as Maybe
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
  [ Case solveA "Test" 33,
    Problem solveA "Problem",
    Case solveBTest "Test" (62 * 56),
    Problem solveBProblem "Problem"
  ]

type Robots = [Int]

type Resources = [Int]

type RobotCost = Resources

type Blueprint = (Int, [RobotCost])

type State = (Robots, Resources)

rGeodes (_, _, _, g) = g

addBot :: Int -> [Int] -> [Int]
addBot idx bots = take idx bots ++ ((bots !! idx) + 1) : drop (idx + 1) bots

readBlueprint :: String -> Blueprint
readBlueprint line = case matchRegex (mkRegex "Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore. Each clay robot costs ([0-9]+) ore. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian.") line of
  Just [bp, aOre, bOre, cOre, cClay, dOre, dObsidian] -> (read bp, [[read aOre, 0, 0, 0], [read bOre, 0, 0, 0], [read cOre, read cClay, 0, 0], [read dOre, 0, read dObsidian, 0]])
  _ -> error "Parse error."

scoreState costs (bots, resources) =
  let oreBotScore = sum (costs !! 0)
      clayBotScore = sum (costs !! 1)
      obsBotScore =
        let obsBotCost = costs !! 2
         in (obsBotCost !! 0) * oreBotScore + (obsBotCost !! 1) * clayBotScore
      geodeBotScore =
        let geodeBotCost = costs !! 3
         in (geodeBotCost !! 0) * oreBotScore + (geodeBotCost !! 2) * obsBotScore
   in sum $ zipWith (*) bots [oreBotScore, clayBotScore, obsBotScore, geodeBotScore]

tryAddOption :: [RobotCost] -> Robots -> Resources -> Int -> Maybe (Robots, Resources)
tryAddOption costs bots resources botIdx =
  let remainingResources = zipWith (-) resources (costs !! botIdx)
   in if {-bots !! botIdx < 5 && -} all (>= 0) remainingResources
        then Just (addBot botIdx bots, remainingResources)
        else Nothing

--then addEveryOption costs (addBot botIdx bots) remainingResources
--else Set.empty

addEveryOption :: [RobotCost] -> Robots -> Resources -> Set (Robots, Resources)
addEveryOption costs bots resources =
  let withoutMoreBot = (bots, resources)
      withMoreBots = Maybe.mapMaybe (tryAddOption costs bots resources) [0, 1, 2, 3]
   in --aaaa = Set.fromList $ drop (length withMoreBots -10) $ sortOn (reverse . fst) $ Set.toList withMoreBots
      Set.fromList (withoutMoreBot : withMoreBots)

getAddBotsOptions :: [RobotCost] -> Resources -> Set (Robots, Resources)
getAddBotsOptions costs resources = addEveryOption costs [0, 0, 0, 0] resources

getNextStatesFromState :: [RobotCost] -> State -> Set State
getNextStatesFromState botCosts (bots, resources) =
  let newBotOptionsWithRemainingResources = getAddBotsOptions botCosts resources
      stateFromAddBotsOption (addedBots, resources) = (zipWith (+) bots addedBots, zipWith (+) resources bots)
      nextStates = Set.map stateFromAddBotsOption newBotOptionsWithRemainingResources
   in nextStates

getStatesAfterMinute :: Int -> [RobotCost] -> Int -> Set State -> Int -> Set State
getStatesAfterMinute bpId botCosts pruneFactor states minute =
  let allNextStates = Set.unions $ Set.map (getNextStatesFromState botCosts) states
      scoredStates = map (\s -> (scoreState botCosts s, s)) $ Set.toList allNextStates
      bestScore = fst $ maximum scoredStates
      nextStates =
        if length allNextStates > 100000
          then (map snd . filter ((> (bestScore * minute) `div` pruneFactor) . fst)) scoredStates
          else Set.toList allNextStates
      bestGeodeCount = maximum $ map (last . snd) nextStates
   in trace (show bpId ++ "-" ++ show minute ++ ": " ++ show (length nextStates) ++ " states. Geodes: " ++ show bestGeodeCount) (Set.fromList nextStates)

evaluateBlueprint :: Int -> Int -> Blueprint -> Int
evaluateBlueprint minuteCount pruneFactor (bpId, botCosts) =
  let states = foldl (getStatesAfterMinute bpId botCosts pruneFactor) (Set.fromList [([1, 0, 0, 0], [0, 0, 0, 0])]) [1 .. minuteCount]
   in maximum $ Set.map (last . snd) states

solveA :: [String] -> Int
solveA lines =
  let maxGeodesPerBlueprint = map (evaluateBlueprint 24 40 . readBlueprint) lines
   in sum $ zipWith (*) maxGeodesPerBlueprint [1 ..]

solveBTest :: [String] -> Int
solveBTest lines =
  let maxGeodesPerBlueprint = map (evaluateBlueprint 32 40 . readBlueprint) (take 3 lines)
   in product maxGeodesPerBlueprint

solveBProblem :: [String] -> Int
solveBProblem lines =
  let maxGeodesPerBlueprint = map (evaluateBlueprint 32 50 . readBlueprint) (take 3 lines)
   in product maxGeodesPerBlueprint

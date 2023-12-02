{-# LANGUAGE TupleSections #-}

module Day16 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import Data.Ix
import Data.List
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
  [ Case solveA "Test" 1651,
    Problem solveA "Problem",
    Case solveB "Test" 1707,
    Problem solveB "Problem"
  ]

type Connection = (String, [String])

type NodePaths = Map String [String]

readValve :: String -> ((String, Int), (String, [String]))
readValve line = case matchRegex (mkRegex "Valve (.*) has flow rate=(.*);.*valves? (.*)") line of
  Just [valve, rateStr, tunnelsStr] -> ((valve, read rateStr), (valve, splitOn ", " tunnelsStr))
  Just _ -> error "Unexpected match result."
  Nothing -> error "Failed to read line."

visitNode :: [Connection] -> String -> [String] -> NodePaths -> NodePaths
visitNode connections node visitedNodes allNodes =
  let currentPath = allNodes Map.! node
      neighbours = snd $ fromJust $ find ((== node) . fst) connections
      updateNeighbour allNodes neighbour =
        let candidatePath = (neighbour : currentPath)
            selectBest neighbourPathMaybe = case neighbourPathMaybe of
              Just neighbourPath -> Just (if length candidatePath < length neighbourPath then candidatePath else neighbourPath)
              Nothing -> Just candidatePath
         in Map.alter selectBest neighbour allNodes
      updatedNodes = foldl updateNeighbour allNodes neighbours
      unvisitedNodes = filter (flip notElem visitedNodes . fst) $ Map.toList updatedNodes
      nextNode = fst $ minimumBy (\a b -> compare (length a) (length b)) unvisitedNodes -- Get next node as unvisited node with shortest path.
   in if null unvisitedNodes
        then updatedNodes
        else visitNode connections nextNode (nextNode : visitedNodes) updatedNodes

getShortestPaths :: [Connection] -> String -> NodePaths
getShortestPaths connections from = visitNode connections from [from] (Map.fromList [(from, [])])

openValve :: Int -> Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> String -> Int
openValve minuteCount valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure valve =
  let valvePressure = valves Map.! valve
      totalPressureAfterMinute = totalPressure + pressurePerMinute
      pressurePerMinuteAfterMinute = pressurePerMinute + valvePressure
   in gotoValves minuteCount valves shortestPaths visitedValves unvisitedValves (minute + 1) pressurePerMinuteAfterMinute totalPressureAfterMinute

gotoValve :: Int -> Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> String -> Int
gotoValve minuteCount valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure toValve =
  let fromValve = head visitedValves
      pathsFromValve = shortestPaths Map.! fromValve
      distanceToValve = length $ pathsFromValve Map.! toValve
   in if minute + distanceToValve < minuteCount -- Enough time to go to, open and release any pressure from the next valve?
        then handleValve minuteCount valves shortestPaths visitedValves unvisitedValves (minute + distanceToValve) pressurePerMinute (totalPressure + distanceToValve * pressurePerMinute) toValve
        else calculateRemainingPressure minuteCount minute pressurePerMinute totalPressure

gotoValves :: Int -> Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> Int
gotoValves minuteCount valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure =
  if null unvisitedValves
    then calculateRemainingPressure minuteCount minute pressurePerMinute totalPressure
    else maximum $ map (gotoValve minuteCount valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure) unvisitedValves

calculateRemainingPressure :: Int -> Int -> Int -> Int -> Int
calculateRemainingPressure minuteCount minute pressurePerMinute totalPressure = totalPressure + pressurePerMinute * (minuteCount + 1 - minute)

handleValve :: Int -> Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> String -> Int
handleValve minuteCount valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure valve =
  let visitedValves' = valve : visitedValves
      unvisitedValves' = filter (/= valve) unvisitedValves
   in if valve `elem` unvisitedValves
        then openValve minuteCount valves shortestPaths visitedValves' unvisitedValves' minute pressurePerMinute totalPressure valve
        else gotoValves minuteCount valves shortestPaths visitedValves' unvisitedValves' minute pressurePerMinute totalPressure

getMaxPressure :: Int -> [(String, Int)] -> Map String NodePaths -> [String] -> Int
getMaxPressure minuteCount valves shortestPaths interestingValves = handleValve minuteCount (Map.fromList valves) shortestPaths [] interestingValves 1 0 0 "AA"

solveA :: [String] -> Int
solveA lines =
  let (valves, connections) = unzip $ map readValve lines
      interestingValves = map fst $ filter ((> 0) . snd) valves
      interestingValvesAndStartingValve = ("AA" : interestingValves)
      shortestPaths =
        Map.fromList $
          zip
            interestingValvesAndStartingValve
            (map (getShortestPaths connections) interestingValvesAndStartingValve)
      highestPressure = getMaxPressure 30 valves shortestPaths interestingValves
   in highestPressure

solveB :: [String] -> Int
solveB lines =
  let (valves, connections) = unzip $ map readValve lines
      interestingValves = map fst $ filter ((> 0) . snd) valves
      interestingValvesAndStartingValve = ("AA" : interestingValves)
      shortestPaths =
        Map.fromList $
          zip
            interestingValvesAndStartingValve
            (map (getShortestPaths connections) interestingValvesAndStartingValve)
      scenarios = map (splitWork interestingValves) [0 .. (2 ^ length interestingValves)]
      scenarioCount = length scenarios
      getScenarioPressure (scenarioIdx, (selfValves, elephantValves)) =
        let selfPressure = getMaxPressure 26 valves shortestPaths selfValves
            elefantPressure = getMaxPressure 26 valves shortestPaths elephantValves
            totalPressure = selfPressure + elefantPressure
         in maybeLogProgress scenarioIdx scenarioCount totalPressure
   in maximum $ map getScenarioPressure scenarios

splitWork :: [String] -> Int -> (Int, ([String], [String]))
splitWork valves scenarioIdx =
  let bits = take (length valves) $ map (\bit -> (scenarioIdx `div` (2 ^ bit)) `mod` 2 == 1) [0 ..] -- Generate bit array with one bit per valve.
      valvesWithBits = zip valves bits
      selfValves = map fst $ filter snd valvesWithBits -- Give true bits to self, false bits to elephant.
      elephantValves = map fst $ filter (not . snd) valvesWithBits
   in (scenarioIdx, (selfValves, elephantValves))

maybeLogProgress scenarioIdx scenarioCount v =
  if scenarioCount > 100 && scenarioIdx `mod` (1 + scenarioCount `div` 100) == 0
    then trace (show (100 * scenarioIdx `div` scenarioCount) ++ "%") v
    else v
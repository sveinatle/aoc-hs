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

openValve :: Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> String -> Int
openValve valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure valve =
  let valvePressure = valves Map.! valve
      totalPressureAfterMinute = totalPressure + pressurePerMinute
      pressurePerMinuteAfterMinute = pressurePerMinute + valvePressure
   in gotoValves valves shortestPaths visitedValves unvisitedValves (minute + 1) pressurePerMinuteAfterMinute totalPressureAfterMinute

gotoValve :: Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> String -> Int
gotoValve valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure toValve =
  let fromValve = head visitedValves
      pathsFromValve = shortestPaths Map.! fromValve
      distanceToValve = length $ pathsFromValve Map.! toValve
   in if minute + distanceToValve < 30 -- Enough time to go to, open and release any pressure from the next valve?
        then handleValve valves shortestPaths visitedValves unvisitedValves (minute + distanceToValve) pressurePerMinute (totalPressure + distanceToValve * pressurePerMinute) toValve
        else calculateRemainingPressure minute pressurePerMinute totalPressure

gotoValves :: Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> Int
gotoValves valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure =
  if null unvisitedValves
    then calculateRemainingPressure minute pressurePerMinute totalPressure
    else maximum $ map (gotoValve valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure) unvisitedValves

calculateRemainingPressure :: Int -> Int -> Int -> Int
calculateRemainingPressure minute pressurePerMinute totalPressure = totalPressure + pressurePerMinute * (31 - minute)

handleValve :: Map String Int -> Map String NodePaths -> [String] -> [String] -> Int -> Int -> Int -> String -> Int
handleValve valves shortestPaths visitedValves unvisitedValves minute pressurePerMinute totalPressure valve =
  let visitedValves' = valve : visitedValves
      unvisitedValves' = filter (/= valve) unvisitedValves
   in if valve `elem` unvisitedValves
        then openValve valves shortestPaths visitedValves' unvisitedValves' minute pressurePerMinute totalPressure valve
        else gotoValves valves shortestPaths visitedValves' unvisitedValves' minute pressurePerMinute totalPressure

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
      highestPressure = handleValve (Map.fromList valves) shortestPaths [] interestingValves 1 0 0 "AA"
   in highestPressure -- trace (intercalate "\n" $ map show (Map.toList shortestPaths)) 0

solveB :: [String] -> Int
solveB lines = 0

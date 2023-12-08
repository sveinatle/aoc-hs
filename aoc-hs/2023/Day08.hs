{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, find, group, groupBy, intersect, maximumBy, nub, sort, sortBy, transpose)
import Data.List.Split (chunksOf, splitEvery, splitOn, splitOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test1" 2, Case solveA "Test2" 6, Problem solveA "Problem", Case solveB "Test3" 6, Problem solveB "Problem"]

data Instruction = L | R

type NodeId = String

data Node = Node {nodeId :: NodeId, left :: NodeId, right :: NodeId}

type Network = Map NodeId Node

data Document = Document {instructions :: [Instruction], network :: Network}

data NavigationStateA = NavigationStateA {location :: NodeId, stepCountA :: Int}

readDocument :: [String] -> Document
readDocument lines =
  let instructions = cycle . map readInstruction . head $ lines
      network = Map.fromList . map ((\node -> (nodeId node, node)) . readNode) . drop 2 $ lines
   in Document {..}
  where
    readInstruction :: Char -> Instruction
    readInstruction 'L' = L
    readInstruction 'R' = R
    readInstruction _ = error "Unexpected instruction."

    readNode :: String -> Node
    readNode line =
      let [nodeId, left, right] = filter (isAlphaNum . head) . groupBy ((==) `on` isAlphaNum) $ line
       in Node {..}

solveA :: [String] -> Int
solveA lines =
  let Document {..} = readDocument lines
   in evalState (move network instructions) NavigationStateA {location = "AAA", stepCountA = 0}

move :: Network -> [Instruction] -> State NavigationStateA Int
move network (i : is) = do
  NavigationStateA {..} <- get
  case location of
    "ZZZ" -> return stepCountA
    _ -> do
      let Node {..} = network Map.! location
      case i of
        L -> put (NavigationStateA {location = left, stepCountA = stepCountA + 1})
        R -> put (NavigationStateA {location = right, stepCountA = stepCountA + 1})
      move network is
move network [] = error "Goal not found before running out of instructions."

data NavigationStateB = NavigationStateB {locations :: ![NodeId], stepCountB :: !Int}

-- TODO: Can type classes be used to have just one implementation, but still handling both parts?
solveB :: [String] -> Int
solveB lines =
  let Document {..} = readDocument lines
      startLocations = filter ((== 'A') . last) . map fst . Map.toList $ network
   in evalState (moveMultiple network instructions) NavigationStateB {locations = startLocations, stepCountB = 0}

moveMultiple :: Network -> [Instruction] -> State NavigationStateB Int
moveMultiple network (i : is) = do
  NavigationStateB {..} <- get
  if all ((== 'Z') . last) $! locations
    then return stepCountB
    else do
      case i of
        L -> put (NavigationStateB {locations = map (left . (network Map.!)) locations, stepCountB = stepCountB + 1})
        R -> put (NavigationStateB {locations = map (right . (network Map.!)) locations, stepCountB = stepCountB + 1})
      moveMultiple network is
moveMultiple network [] = error "Goal not found before running out of instructions."

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where

import Control.Monad
import Control.Monad.State
import Data.Char (digitToInt, isAlpha, isDigit, isSpace)
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

cases = [Case solveA "Test1" 2, Case solveA "Test2" 6, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

data Instruction = L | R

type NodeId = String

data Node = Node {nodeId :: NodeId, left :: NodeId, right :: NodeId}

type Network = Map NodeId Node

data Document = Document {instructions :: [Instruction], network :: Network}

data NavigationState = NavigationState {location :: NodeId, stepCount :: Int}

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
      let [nodeId, left, right] = filter (isAlpha . head) . groupBy ((==) `on` isAlpha) $ line
       in Node {..}

solveA :: [String] -> Int
solveA lines =
  let Document {..} = readDocument lines
   in evalState (move network instructions) NavigationState {location = "AAA", stepCount = 0}

move :: Network -> [Instruction] -> State NavigationState Int
move network (i : is) = do
  NavigationState {..} <- get
  case location of
    "ZZZ" -> return stepCount
    _ -> do
      let Node {..} = network Map.! location
      case i of
        L -> put (NavigationState {location = left, stepCount = stepCount + 1})
        R -> put (NavigationState {location = right, stepCount = stepCount + 1})
      move network is
move network [] = error "Goal not found before running out of instructions."

solveB :: [String] -> Int
solveB l = 0

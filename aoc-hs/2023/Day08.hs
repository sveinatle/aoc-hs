{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day08 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, find, group, groupBy, intersect, maximumBy, minimumBy, nub, sort, sortBy, transpose)
import Data.List.Split (chunksOf, splitEvery, splitOn, splitOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [CaseStr solveA "Test1" "2", CaseStr solveA "Test2" "6", ProblemStr solveA "Problem", CaseStr solveB "Test3" "6", ProblemStr solveB "Problem"]

type InstructionIdx = Integer

data Instruction = L | R deriving (Show)

type NodeId = String

data Node = Node {nodeId :: NodeId, left :: NodeId, right :: NodeId} deriving (Show)

type Nodes = Map NodeId Node

data Document = Document {instructions :: Vector Instruction, nodes :: Nodes} deriving (Show)

type Steps = Integer

type StartPoint = (NodeId, InstructionIdx)

type NextEnding = (NodeId, Steps)

type IsEndNodeTest = NodeId -> Bool

data World = World
  { document :: Document, -- Unchanging problem definition.
    nextEndingsCache :: Map StartPoint NextEnding -- Cache: Given a certain node and instruction idx, remember the number of steps to the next ending node.
  }
  deriving (Show)

newtype TraversalMonad a = TraversalMonad {runTraversal :: World -> (a, World)}

instance Functor TraversalMonad where
  fmap :: (a -> b) -> TraversalMonad a -> TraversalMonad b
  fmap f (TraversalMonad g) = TraversalMonad $ \w -> let (x, w') = g w in (f x, w')

instance Applicative TraversalMonad where
  pure :: a -> TraversalMonad a
  pure x = TraversalMonad $ \w -> (x, w)
  (<*>) :: TraversalMonad (a -> b) -> TraversalMonad a -> TraversalMonad b
  (TraversalMonad f) <*> (TraversalMonad g) = TraversalMonad $ \w ->
    let (func, w') = f w
        (x, w'') = g w'
     in (func x, w'')

instance Monad TraversalMonad where
  return :: a -> TraversalMonad a
  return x = TraversalMonad $ \w -> (x, w)

  (>>=) :: TraversalMonad a -> (a -> TraversalMonad b) -> TraversalMonad b
  (TraversalMonad f) >>= g = TraversalMonad $ \w ->
    let (x, w') = f w -- Take computation 'f' from existing monad and apply to some initial world 'w'.
        TraversalMonad h = g x -- Use 'g' to create the next part of the computation, independent of the existing computation f.
     in h w' -- Apply the new part of the computation (h) to the world returned by the existing computation to compose the two computation into a new computation.

getNextEnding :: IsEndNodeTest -> (NodeId, InstructionIdx) -> TraversalMonad (NodeId, Steps)
getNextEnding isEndNode (nodeId, instructionIdx) = TraversalMonad computation
  where
    computation world = case Map.lookup (nodeId, wrappedInstructionIdx) nextEndingsCache of
      -- Already had the result in the cache. Just return existing result without updating the cache.
      Just result -> (result, world)
      -- No cached result. Recursively get result for next step, compute result for this step, save in cache, and then return.
      Nothing -> getNextEndingFromThisNode (nodeId, wrappedInstructionIdx) world
      where
        World {document, nextEndingsCache} = world
        Document {instructions, nodes} = document

        wrappedInstructionIdx = instructionIdx `mod` fromIntegral (V.length instructions)

        getNextEndingFromThisNode :: StartPoint -> World -> (NextEnding, World)
        getNextEndingFromThisNode startPoint world =
          let ((endingNodeId, stepsFromNextNode), World {nextEndingsCache = nextEndingsCacheWithNextNode}) = getNextEndingFromNextNode startPoint world
              stepsFromThisNode = stepsFromNextNode + 1
              nextEnding = (endingNodeId, stepsFromThisNode)
              -- Add next ending from this node/instruction to cache.
              nextEndingsCacheWithThisNode = Map.insert startPoint nextEnding nextEndingsCacheWithNextNode
           in trace ("Added " ++ show startPoint ++ " -> " ++ show nextEnding) (nextEnding, World {document, nextEndingsCache = nextEndingsCacheWithThisNode})

        -- Run traversal from the next node, returning info about the next ending and an updated cache.
        getNextEndingFromNextNode :: StartPoint -> World -> (NextEnding, World)
        getNextEndingFromNextNode (nodeId, instructionIdx) world =
          let Node {left, right} = nodes Map.! nodeId
              nextNodeId = case instructions V.! fromInteger instructionIdx of
                L -> left
                R -> right
              nextInstructionIdx = (instructionIdx + 1)
           in if isEndNode nextNodeId
                then ((nextNodeId, 0), world)
                else runTraversal (getNextEnding isEndNode (nextNodeId, nextInstructionIdx)) world

readDocument :: [String] -> Document
readDocument lines =
  let instructions = V.fromList . map readInstruction . head $ lines
      nodeList = map ((\node -> (nodeId node, node)) . readNode) . drop 2 $ lines
      nodes = Map.fromList nodeList
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

solveA :: [String] -> String
solveA lines =
  let document@Document {nodes} = readDocument lines
      startNodeId = head . filter isStartNode . Map.keys $ nodes
      ((endNodeId, steps), _) = runTraversal (getNextEnding isEndNode (startNodeId, 0)) (World document Map.empty)
   in show steps
  where
    isStartNode = (== "AAA")
    isEndNode = (== "ZZZ")

type Traversal = (NodeId, Steps)

-- TODO: Detect cycles and find LCM even though not technically correct?
solveB :: [String] -> String
solveB lines =
  let document@Document {nodes} = readDocument lines
      initialTraversals = [(nodeId, 0) | nodeId <- Map.keys nodes, isStartNode nodeId]
      initialWorld = World document Map.empty
   in show $ findSynchronizedEnd initialTraversals initialWorld
  where
    isStartNode = (== 'A') . last
    isEndNode = (== 'Z') . last

    findSynchronizedEnd :: [Traversal] -> World -> Integer
    findSynchronizedEnd traversals world
      -- Debug: | (> 10) . snd . head $ traversals = error "abort"
      | allAtEndWithSameSteps traversals = snd $ head traversals
      | otherwise =
        let shortestTraversal = selectShortestTraversal traversals
            (updatedTraversal, updatedWorld) = moveTraversal shortestTraversal world
            updatedTraversals = updateTraversals traversals shortestTraversal updatedTraversal
         in findSynchronizedEnd updatedTraversals updatedWorld

    allAtEndWithSameSteps :: [Traversal] -> Bool
    allAtEndWithSameSteps ts = all (isEndNode . fst) ts && length (nub (map snd ts)) == 1

    selectShortestTraversal :: [Traversal] -> Traversal
    selectShortestTraversal = minimumBy (comparing snd)

    moveTraversal :: Traversal -> World -> (Traversal, World)
    moveTraversal (nodeId, stepsSoFar) world =
      let ((nextNodeId, stepsToNext), newWorld) = runTraversal (getNextEnding isEndNode (nodeId, stepsSoFar)) world
       in ((nextNodeId, stepsSoFar + stepsToNext), newWorld)

    updateTraversals :: [Traversal] -> Traversal -> Traversal -> [Traversal]
    updateTraversals ts oldT updatedT = updatedT : filter (/= oldT) ts -- This will also remove any other traversals that are the same, but that shouldn't be a problem.
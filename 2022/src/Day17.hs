{-# LANGUAGE TupleSections #-}

module Day17 where

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
  [ Case solveA "Test" 3068,
    Problem solveA "Problem",
    --Case solveB "Test" 1514285714288, -- Doesn't seem like test case loops like the problem case?
    Problem solveB "Problem"
  ]

showBool x = if x then '#' else '.'

showStructure :: [[Bool]] -> [Char]
showStructure structure = intercalate "\n" ((map . map) showBool structure)

type Jet = Char

type Shape = [[Bool]]

type Row = [Bool]

type Chamber = [Row]

data State = State {sChamber :: Chamber, sRockCount :: Int, sCycleInfo :: [(Int, Int)], sShapes :: [Shape], sJets :: [Jet]}

instance Show State where
  show (State chamber _ _ _ _) = intercalate "\n" ((map . map) showBool chamber)

data RockState = RockState {rState :: State, rShape :: Shape, rX :: Int, rY :: Int, rDone :: Bool}

instance Show RockState where
  show (RockState s shape x y done) = "Rock: " ++ show x ++ " " ++ show y ++ " " ++ show done ++ ":\n" ++ showStructure shape

clamp from to x = min to (max from x)

padShape :: Shape -> Int -> Shape
padShape shape x =
  let shapeWidth = maximum $ map length shape
      before = replicate x False
      after = replicate ((7 - shapeWidth) - x) False
   in map (\shapeLine -> before ++ shapeLine ++ after) shape

isPlacementOk :: (State, Shape, Int, Int) -> Bool
isPlacementOk (s, shape, x, y)
  | y < 0 = t'' "isPlacementOk: y < 0" False
  | x < 0 = t'' "isPlacementOk: x < 0" False
  | x + shapeWidth > 7 = t'' "isPlacementOk: x + shapeWidth > 7" False
  | otherwise =
    let rows = sChamber s
        rowCount = length rows
        skippedRowCount = max 0 (rowCount - (y + shapeHeight))
        overlappingRowCount = clamp 0 shapeHeight (rowCount - y)
        overlappingRows = (take overlappingRowCount . drop skippedRowCount) rows
        virtualRowsAbove = replicate (shapeHeight - overlappingRowCount) (replicate 7 False)
        paddedShape = padShape shape x
        visualization =
          "isPlacementOk: \n" ++ showStructure paddedShape
            ++ "\n-- ("
            ++ show x
            ++ ","
            ++ show y
            ++ ") "
            ++ show skippedRowCount
            ++ "/"
            ++ show overlappingRowCount
            ++ " --\n"
            ++ showStructure overlappingRows
            ++ "\n========== "
     in t'' visualization $ notElem (True, True) (zip (concat (virtualRowsAbove ++ overlappingRows)) (concat paddedShape))
  where
    shapeWidth = maximum $ map length shape
    shapeHeight = length shape
    t'' txt v = v

place :: State -> Shape -> Int -> Int -> State
place (State chamber rockCount cycleLog shapes jets) shape x y
  | y < 0 = error "Too lo w."
  | x < 0 = error "Too left."
  | x + shapeWidth > 7 = error "Too right."
  | otherwise =
    let rows = chamber
        rowCount = length rows
        skippedRowCount = max 0 (rowCount - (y + shapeHeight))
        overlappingRowCount = clamp 0 shapeHeight (rowCount - y)
        overlappingRows = (take overlappingRowCount . drop skippedRowCount) rows
        virtualRowsAbove = replicate (shapeHeight - overlappingRowCount) (replicate 7 False)
        paddedShape = padShape shape x
        mergedOverlappingRows = chunksOf 7 $ zipWith (||) (concat (virtualRowsAbove ++ overlappingRows)) (concat paddedShape)
        finalRows = take skippedRowCount rows ++ mergedOverlappingRows ++ drop (overlappingRowCount + skippedRowCount) rows
     in State finalRows rockCount cycleLog shapes jets
  where
    shapeWidth = maximum $ map length shape
    shapeHeight = length shape

takeJet :: State -> (State, Jet)
takeJet (State chamber rockCount cycleLog shapes jets) = (State chamber rockCount cycleLog shapes (tail jets), head jets)

takeShape :: State -> (State, Shape)
takeShape (State chamber rockCount cycleLog shapes jets) = (State chamber (rockCount + 1) cycleLog (tail shapes) jets, head shapes)

shapeDefinitions :: [Shape]
shapeDefinitions =
  (map . map . map)
    (== '#')
    [ ["####"],
      [ ".#.",
        "###",
        ".#."
      ],
      [ "..#",
        "..#",
        "###"
      ],
      [ "#",
        "#",
        "#",
        "#"
      ],
      [ "##",
        "##"
      ]
    ]

solveA :: [String] -> Int
solveA lines =
  let jets = cycle (head lines)
      shapes = cycle shapeDefinitions
      rockCount = 2022
      initialState = State [] 0 [] shapes jets
   in length $ sChamber $ iterate simulateNextRock initialState !! rockCount

simulateNextRock :: State -> State
simulateNextRock state =
  let (state', shape) = takeShape state
      initialX = 2
      initialY = (length (sChamber state') + 3)
      initialRockState = RockState state' shape initialX initialY False
      finalRockState = until rDone (tryMoveDown . simulateJet . trackCycle) initialRockState
   in place (rState finalRockState) shape (rX finalRockState) (rY finalRockState)

trackCycle :: RockState -> RockState
trackCycle (RockState (State chamber rockCount cycleLog shapes jets) shape x y done) =
  let visualization =
        "================\nHeight: "
          ++ (show . length) chamber -- 2796
          ++ "\nRock count: "
          ++ show rockCount -- 1750
          ++ "\nNext shape:\n"
          ++ (showStructure . head) shapes
          ++ "\nTop rows:\n"
          ++ (showStructure . take 5) chamber
      t'' txt v = v
   in if head jets == '?'
        then t'' visualization (RockState (State chamber rockCount ((rockCount, length chamber) : cycleLog) shapes (tail jets)) shape x y done)
        else RockState (State chamber rockCount cycleLog shapes jets) shape x y done

simulateJet :: RockState -> RockState
simulateJet (RockState s shape x y done) =
  let (s', jet) = takeJet s
      x' = case jet of
        '<' -> x - 1
        '>' -> x + 1
        _ -> error "Unexpected jet input."
   in if isPlacementOk (s', shape, x', y)
        then RockState s' shape x' y False
        else RockState s' shape x y False

tryMoveDown :: RockState -> RockState
tryMoveDown (RockState s shape x y done) =
  if isPlacementOk (s, shape, x, y - 1)
    then RockState s shape x (y - 1) False
    else RockState s shape x y True

solveB :: [String] -> Int
solveB lines =
  let jets = cycle (head lines ++ ['?'])
      shapes = cycle shapeDefinitions
      targetRockCount = 1000000000000
      initialState = State [] 0 [] shapes jets
   in computeHeight targetRockCount $ until (canComputeHeight targetRockCount) simulateNextRock initialState

computeHeight :: Int -> State -> Int
computeHeight targetRockCount (State chamber rockCount cycleLog _ _) = case maybeGetCycleStats cycleLog of
  Nothing -> error "Cannot compute stats"
  Just (rocksPerCycle, rowsPerCycle) ->
    let remainingRocks = targetRockCount - rockCount
        rowCount = length chamber
     in rowCount + (remainingRocks `div` rocksPerCycle) * rowsPerCycle

canComputeHeight :: Int -> State -> Bool
canComputeHeight targetRockCount (State chamber rockCount cycleLog _ _) = case maybeGetCycleStats cycleLog of
  Nothing -> False
  Just (rocksPerCycle, _) ->
    let remainingRocks = targetRockCount - rockCount
     in remainingRocks `mod` rocksPerCycle == 0

maybeGetCycleStats :: [(Int, Int)] -> Maybe (Int, Int)
maybeGetCycleStats cycleLog =
  if length cycleLog >= 2
    then
      let rocksPerCycle = (fst . head) cycleLog - (fst . head . tail) cycleLog
          rowsPerCycle = (snd . head) cycleLog - (snd . head . tail) cycleLog
       in Just (rocksPerCycle, rowsPerCycle)
    else Nothing
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
    Case solveB "Test" 1514285714288,
    Problem solveB "Problem"
  ]

showBool x = if x then '#' else '.'

showStructure :: [[Bool]] -> [Char]
showStructure structure = intercalate "\n" ((map . map) showBool structure)

type Jet = Char

type Shape = [[Bool]]

type Row = [Bool]

type Chamber = [Row]

data State = State {sChamber :: Chamber, sShapes :: [Shape], sJets :: [Jet]}

instance Show State where
  show (State chamber _ _) = intercalate "\n" ((map . map) showBool chamber)

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
place (State chamber shapes jets) shape x y
  | y < 0 = error "Too low."
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
     in State finalRows shapes jets
  where
    shapeWidth = maximum $ map length shape
    shapeHeight = length shape

takeJet :: State -> (State, Jet)
takeJet (State chamber shapes jets) = (State chamber shapes (tail jets), head jets)

takeShape :: State -> (State, Shape)
takeShape (State chamber shapes jets) = (State chamber (tail shapes) jets, head shapes)

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
      maxHeight = 4 * rockCount + 3
      chamber = []
      state = State chamber shapes jets
   in length $ sChamber $ iterate simulateNextRock state !! rockCount

simulateNextRock :: State -> State
simulateNextRock state =
  let (state', shape) = takeShape state
      initialX = 2
      initialY = (length (sChamber state') + 3)
      initialRockState = RockState state' shape initialX initialY False
      finalRockState = until rDone (tryMoveDown . simulateJet) initialRockState
   in place (rState finalRockState) shape (rX finalRockState) (rY finalRockState)

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
solveB lines = 999

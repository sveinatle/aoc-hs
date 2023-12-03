{-# LANGUAGE TupleSections #-}

module Day03 where

import Data.Char (digitToInt, isDigit, isNumber)
import Data.List (foldl', groupBy, sort, sortBy)
import Data.Maybe (mapMaybe)
import DayProblem
import Debug.Trace
import Text.Read (readMaybe)

log2 v = trace (show v) v

logList :: Show v => [v] -> [v]
logList vs = trace (concatMap (\v -> '\n' : show v) vs) vs

cases = [Case solveA "Test" 4361, Problem solveA "Problem", Case solveB "Test" 467835, Problem solveB "Problem"]

data Schematic = Schematic {w :: Int, h :: Int, rows :: [String]} deriving (Show)

data NumberLocation = NumberLocation {nlX :: Int, nlY :: Int, nlColSpan :: Int, nlValue :: Int} deriving (Show)

solveA :: [String] -> Int
solveA lines =
  let schematic = readSchematic lines
      numbers = findNumbersInSchematic schematic
   in sum . map nlValue . filter (isPartNumber schematic) $ numbers

readSchematic :: [String] -> Schematic
readSchematic lines =
  let w = length $ head lines
      h = length lines
   in Schematic w h lines

getCell :: Schematic -> Int -> Int -> Char
getCell (Schematic w h rows) x y =
  if x >= 0 && x < w && y >= 0 && y < h
    then (rows !! y) !! x
    else '.'

findNumbersInSchematic :: Schematic -> [NumberLocation]
findNumbersInSchematic (Schematic w h rows) =
  let numbersInRows = map findNumbersInRow rows
      toNumberLocation y = map (\(x, colSpan, number) -> NumberLocation x y colSpan number)
   in concat $ zipWith toNumberLocation [0 ..] numbersInRows

findNumbersInRow :: String -> [(Int, Int, Int)]
findNumbersInRow row =
  let groups = groupBy (\a b -> isDigit a == isDigit b) row
      groupsWithLocation = scanl (\(prevX, prevColSpan, _) group -> (prevX + prevColSpan, length group, readMaybe group :: Maybe Int)) (0, 0, Nothing) groups
   in mapMaybe
        ( \(x, colSpan, maybeNumber) -> case maybeNumber of
            Just number -> Just (x, colSpan, number)
            _ -> Nothing
        )
        groupsWithLocation

getNeighbours :: Schematic -> NumberLocation -> [(Int, Int)]
getNeighbours schematic (NumberLocation x y colSpan number) =
  let xs = [(x - 1) .. (x + colSpan)]
      above = zip xs (repeat (y - 1))
      below = zip xs (repeat (y + 1))
      sides = [(x - 1, y), (x + colSpan, y)]
   in sides ++ above ++ below

isPart :: Char -> Bool
isPart c = not (isDigit c || c == '.')

isPartNumber :: Schematic -> NumberLocation -> Bool
isPartNumber schematic numbers =
  let neighbours = getNeighbours schematic numbers
   in any (isPart . uncurry (getCell schematic)) neighbours

isGear :: Char -> Bool
isGear c = c == '*'

getNeighbourGears :: Schematic -> NumberLocation -> [((Int, Int), Int)]
getNeighbourGears schematic number =
  let neighbours = getNeighbours schematic number
      isGear (x, y) = (== '*') $ getCell schematic x y
   in map (,nlValue number) $ filter isGear neighbours

solveB :: [String] -> Int
solveB lines =
  let schematic = readSchematic lines
      numbers = findNumbersInSchematic schematic
      gearsWithNumber = concatMap (getNeighbourGears schematic) numbers
      groupedGearsWithNumbers = groupBy (\((x1, y1), _) ((x2, y2), _) -> x1 == x2 && y1 == y2) $ sort gearsWithNumber
      calculateGearPower [(_, num1), (_, num2)] = num1 * num2
      calculateGearPower _ = error "Unexpected gear numbers length."
   in (sum . map calculateGearPower . filter ((== 2) . length)) groupedGearsWithNumbers

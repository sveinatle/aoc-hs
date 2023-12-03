module Day03 where

import Data.Char (digitToInt, isDigit, isNumber)
import Data.List (foldl')
import DayProblem
import Debug.Trace

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
  let xWithCell = zip [0 ..] row
      isAdjacent (prevX, prevColSpan, prevNumber) nextX = prevX + prevColSpan == nextX
      addDigit ((prevX, prevColSpan, prevNumber) : numbers) digit = (prevX, prevColSpan + 1, prevNumber * 10 + digit) : numbers
      addDigit [] _ = error "Should never happen."
   in foldl'
        ( \numbers (x, cell) ->
            if isDigit cell
              then
                if not (null numbers) && (isAdjacent . head) numbers x -- If a number has been found already and it is adjacent to this cell.
                  then addDigit numbers (digitToInt cell) -- Then update the existing number.
                  else (x, 1, digitToInt cell) : numbers -- Else prepend the digit as a new number.
              else numbers
        )
        []
        xWithCell

isPart :: Char -> Bool
isPart c = not (isDigit c || c == '.')

isPartNumber :: Schematic -> NumberLocation -> Bool
isPartNumber schematic (NumberLocation x y colSpan number) =
  let xs = [(x - 1) .. (x + colSpan)]
      above = zip xs (repeat (y - 1))
      below = zip xs (repeat (y + 1))
      sides = [(x - 1, y), (x + colSpan, y)]
      neighbours = sides ++ above ++ below
   in any (isPart . uncurry (getCell schematic)) neighbours

solveB :: [String] -> Int
solveB lines = 0

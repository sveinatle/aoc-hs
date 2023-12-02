module Day02 where

import Data.Char (digitToInt, isDigit, isNumber)
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import DayProblem
import Debug.Trace

log2 v = trace (show v) v

cases = [Case solveA "Test" 8, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

-- 12 red cubes, 13 green cubes, and 14 blue cubes
data Game = Game {gameId :: Int, gameDraws :: [Draw]} deriving (Show)

data Draw = Draw {red :: Int, green :: Int, blue :: Int} deriving (Show)

solveA :: [String] -> Int
solveA = sum . map gameId . filter isPossibleGame . map readGame

readGame :: String -> Game
readGame line =
  let [textId, textDraws] = splitOn ": " line
   in Game {gameId = readGameId textId, gameDraws = readDraws textDraws}

readGameId = read . last . splitOn " "

readDraws = map readDraw . splitOn "; "

readDraw = combineDraws (+) . map readCubes . splitOn ", "

readCubes text = case splitOn " " text of
  [num, "red"] -> Draw {red = read num, green = 0, blue = 0}
  [num, "green"] -> Draw {red = 0, green = read num, blue = 0}
  [num, "blue"] -> Draw {red = 0, green = 0, blue = read num}
  _ -> error "Failed to parse."

combineDraws :: Foldable t => (Int -> Int -> Int) -> t Draw -> Draw
combineDraws combine =
  let combineField field acc next = combine (field acc) (field next)
   in foldl
        ( \acc next ->
            (Draw {red = combineField red acc next, green = combineField green acc next, blue = combineField blue acc next})
        )
        (Draw {red = 0, green = 0, blue = 0})

isPossibleGame :: Game -> Bool
isPossibleGame (Game id draws) =
  let Draw maxRed maxGreen maxBlue = combineDraws max draws
   in (maxRed <= 12) && (maxGreen <= 13) && (maxBlue <= 14)

solveB :: [String] -> Int
solveB lines = 1
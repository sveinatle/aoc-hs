module Day04 where

import Data.List (transpose, union)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing)
import DayProblem
import Debug.Trace (trace)

problems = (P solveA 4512, P solveB 1924)

solveA :: [String] -> Int
solveA lines =
  let (draws, boards) = readProblem lines
   in drawUntilFirstWinner draws boards

solveB :: [String] -> Int
solveB lines =
  let (draws, boards) = readProblem lines
   in drawUntilLastWinner draws boards

newtype Board = B [[Int]] deriving (Show)

readProblem :: [String] -> ([Int], [Board])
readProblem lines =
  let (drawsLine : allBoardLines) = lines
      drawWords = splitOn "," drawsLine
      draws = map read drawWords :: [Int]
      boards = readBoards allBoardLines
   in (draws, boards)

readBoards :: [String] -> [Board]
readBoards [] = []
readBoards lines =
  let boardLines = take 5 (drop 1 lines)
      remainingBoardLines = drop 6 lines
      board = readBoard boardLines
   in (board : readBoards remainingBoardLines)

readBoard :: [String] -> Board
readBoard lines =
  let readBoardLine line = map read (words line) :: [Int]
      rows = map readBoardLine lines
      columns = transpose rows
   in B (rows ++ columns)

drawUntilFirstWinner :: [Int] -> [Board] -> Int
drawUntilFirstWinner [] _ = error "no more draws"
drawUntilFirstWinner (draw : draws) boards =
  let newBoards = updateBoards draw boards
      winner = checkBoards newBoards
   in case winner of
        Nothing -> drawUntilFirstWinner draws newBoards
        Just score -> draw * score

drawUntilLastWinner :: [Int] -> [Board] -> Int
drawUntilLastWinner [] _ = error "no more draws"
drawUntilLastWinner (draw : draws) boards =
  let newBoards = updateBoards draw boards
      ongoingBoards = filter (not . isWinner) newBoards
   in case ongoingBoards of -- Keep drawing until all boards have won, then return score for last board
        [] -> draw * scoreBoard (head newBoards)
        _ -> drawUntilLastWinner draws ongoingBoards

updateBoards :: Int -> [Board] -> [Board]
updateBoards draw = map (updateBoard draw)

updateBoard :: Int -> Board -> Board
updateBoard draw (B groups) = B (map (filter (/= draw)) groups)

checkBoards :: [Board] -> Maybe Int
checkBoards [] = Nothing
checkBoards (b : boards) =
  if isWinner b
    then Just (scoreBoard b)
    else checkBoards boards

isWinner :: Board -> Bool
isWinner (B groups) = any null groups

scoreBoard :: Board -> Int
scoreBoard (B groups) = sum (unique groups)
  where
    unique = foldr union []
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day07 where

import Data.Char (digitToInt, isDigit, isSpace)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, find, group, groupBy, intersect, nub, sort, sortBy, transpose)
import Data.List.Split (chunksOf, splitEvery, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 6440, Problem solveA "Problem", Case solveB "Test" 999, Problem solveB "Problem"]

data Hand = Hand {handValue :: Int, bid :: Int, cards :: [Char]} deriving (Ord, Eq, Show)

solveA :: [String] -> Int
solveA = calculateTotalScore . readHands
  where
    calculateTotalScore :: [Hand] -> Int
    calculateTotalScore = sum . zipWith (\rank Hand {bid} -> rank * bid) [1 ..] . sort

readHands :: [[Char]] -> [Hand]
readHands = map readHand
  where
    readHand line =
      let [cards, bidString] = words line
          handValue = sum (zipWith (\idx cardValue -> (15 ^ idx) * cardValue) [6, 5 .. 1] (handTypeValue cards : map cardValue cards))
          bid = read bidString
       in Hand {..}

    cardValue :: Char -> Int
    cardValue card = case card of
      'A' -> 14
      'K' -> 13
      'Q' -> 12
      'J' -> 11
      'T' -> 10
      _ -> digitToInt card

    handTypeValue :: [Char] -> Int
    handTypeValue cards = case reverse . sort . map length . group . sort $ cards of
      (5 : _) -> 6
      (4 : _) -> 5
      (3 : 2 : _) -> 4
      (3 : _) -> 3
      (2 : 2 : _) -> 2
      (2 : _) -> 1
      _ -> 0

solveB :: [String] -> Int
solveB lines = 0
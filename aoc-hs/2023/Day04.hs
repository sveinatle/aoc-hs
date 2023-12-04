{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day04 where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, groupBy, intersect, nub, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 13, Problem solveA "Problem", Case solveB "Test" 30, Problem solveB "Problem"]

data Card = Card {matchCount :: Int, cardCount :: Int}

solveA :: [String] -> Int
solveA = sum . map (calculateScore . matchCount . readCard . T.pack)
  where
    calculateScore :: Int -> Int
    calculateScore matchCount = if matchCount == 0 then 0 else 2 ^ (matchCount -1)

readCard :: T.Text -> Card
readCard line =
  let [cardIdString, winningString, mineString] = map T.strip $ T.split (`elem` [':', '|']) line
      winning = readNumbers winningString
      mine = readNumbers mineString
      matchCount = countMatches winning mine
   in Card {matchCount, cardCount = 1}
  where
    readNumbers :: T.Text -> [Int]
    readNumbers = map (read . T.unpack) . T.words

    countMatches :: [Int] -> [Int] -> Int
    countMatches winning mine = length $ intersect winning mine

type CardId = Int

type Cards = Map CardId Card

solveB :: [String] -> Int
solveB = countCards . processWinnings . readCardsMap
  where
    readCardsMap :: [String] -> Cards
    readCardsMap = Map.fromList . zip [1 ..] . map (readCard . T.pack)

    processWinnings :: Cards -> Cards
    processWinnings cards = foldl processCardId cards (take (length cards) [1 ..])

    processCardId :: Cards -> CardId -> Cards
    processCardId cards cardId =
      let card = cards Map.! cardId
          gainedCount = cardCount card
          gainedCardIds = [cardId + 1 .. cardId + matchCount card]

          updateCard :: Cards -> CardId -> Cards
          updateCard cards cardId = Map.adjust (\Card {cardCount = oldCount, ..} -> Card {cardCount = oldCount + gainedCount, ..}) cardId cards
       in foldl updateCard cards gainedCardIds

    countCards :: Cards -> Int
    countCards = sum . Map.map cardCount

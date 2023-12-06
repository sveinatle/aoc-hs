{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day05 where

import Data.Char (isDigit, isSpace)
import Data.List (dropWhileEnd, find, groupBy, intersect, nub, sort, sortBy)
import Data.List.Split (chunksOf, splitEvery, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Text as T
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 35, Problem solveA "Problem", Case solveB "Test" 46, Problem solveB "Problem"]

data MapRange = MapRange {srcStart :: Int, destStart :: Int, count :: Int} deriving (Show, Eq, Ord)

newtype Mapping = Mapping [MapRange] deriving (Show, Eq, Ord)

solveA :: [String] -> Int
solveA = minimum . mapSeedsToLocation . readProblem
  where
    mapSeedsToLocation :: ([Int], [Mapping]) -> [Int]
    mapSeedsToLocation (seeds, mappings) = map (mapSeedToLocation mappings) seeds

    mapSeedToLocation :: [Mapping] -> Int -> Int
    mapSeedToLocation mappings seed = foldl applyMapping seed mappings

    applyMapping seed (Mapping ranges) = case find (isSeedInRange seed) ranges of
      Just MapRange {..} -> destStart + seed - srcStart
      Nothing -> seed

    isSeedInRange :: Int -> MapRange -> Bool
    isSeedInRange seed MapRange {..} = seed >= srcStart && seed - srcStart < count

readProblem :: [String] -> ([Int], [Mapping])
readProblem lines =
  let seeds = map read . tail . words . head $ lines
      mappings = map readMapping . splitOn [""] . filter (notElem ':') . drop 2 $ lines
   in (seeds, mappings)
  where
    readMapping :: [String] -> Mapping
    readMapping lines = Mapping . sort . map readRange $ lines

    readRange :: String -> MapRange
    readRange line =
      case map read $ words line of
        [destStart, srcStart, count] -> MapRange {..}
        _ -> error $ "Expected 3 words. Got: " ++ line

data SeedRange = SeedRange {seedStart :: Int, seedEnd :: Int} deriving (Show, Eq, Ord)

solveB :: [String] -> Int
solveB lines =
  let (seeds, mappings) = readProblem lines
      initialSeedRanges = sort $ map (\[seedStart, count] -> SeedRange {seedStart, seedEnd = seedStart + count -1}) $ chunksOf 2 seeds
      nearestSeedRange = minimum $ foldl applyMappingToSeedRanges initialSeedRanges mappings
   in seedStart nearestSeedRange
  where
    applyMappingToSeedRanges :: [SeedRange] -> Mapping -> [SeedRange]
    applyMappingToSeedRanges seedRanges (Mapping mapRanges) = sort $ applyMapRangesToSeedRanges seedRanges mapRanges

    applyMapRangesToSeedRanges :: [SeedRange] -> [MapRange] -> [SeedRange]
    applyMapRangesToSeedRanges [] _ = []
    applyMapRangesToSeedRanges seedRanges [] = seedRanges
    applyMapRangesToSeedRanges seedRanges mapRanges =
      let splitAfterPosition = getSplitAfterPosition (head seedRanges) (head mapRanges)
          (choppedOffSeedRange, remainingSeedRanges) = splitSeedRanges seedRanges splitAfterPosition
          (choppedOffMapRange, remainingMapRanges) = splitMapRanges mapRanges splitAfterPosition
       in mapSeedRange choppedOffSeedRange choppedOffMapRange ++ applyMapRangesToSeedRanges remainingSeedRanges remainingMapRanges

    -- Might be nicer to only accept a single seed range since this function only needs the next range. But then using it would be more messy.
    splitSeedRanges :: [SeedRange] -> Int -> (Maybe SeedRange, [SeedRange])
    splitSeedRanges [] _ = error "Should be at least one seed to split."
    splitSeedRanges (seedRange : seedRanges) splitAfterPosition
      | splitAfterPosition < seedStart = (Nothing, seedRange : seedRanges)
      | splitAfterPosition >= seedEnd = (Just seedRange, seedRanges)
      | otherwise =
        ( Just SeedRange {seedStart, seedEnd = splitAfterPosition},
          SeedRange {seedStart = splitAfterPosition + 1, seedEnd} : seedRanges
        )
      where
        SeedRange {seedStart, seedEnd} = seedRange

    splitMapRanges :: [MapRange] -> Int -> (Maybe MapRange, [MapRange])
    splitMapRanges [] _ = error "Should be at least one map range to split."
    splitMapRanges (mapRange : mapRanges) splitAfterPosition
      | splitAfterPosition < mapSrcStart = (Nothing, mapRange : mapRanges)
      | splitAfterPosition >= mapSrcEnd = (Just mapRange, mapRanges)
      | otherwise =
        ( Just MapRange {srcStart, destStart, count = splitAfterPosition - srcStart + 1},
          MapRange
            { srcStart = splitAfterPosition + 1,
              destStart = splitAfterPosition + 1 + movement,
              count = mapSrcEnd - splitAfterPosition
            } :
          mapRanges
        )
      where
        MapRange {srcStart, destStart, count} = mapRange
        mapSrcStart = srcStart
        mapSrcEnd = srcStart + count -1
        movement = destStart - srcStart

    -- Using list return values instead of Maybe just to make it easy to concatenate.
    mapSeedRange :: Maybe SeedRange -> Maybe MapRange -> [SeedRange]
    mapSeedRange (Just seedRange) (Just MapRange {srcStart, destStart}) =
      [ seedRange
          { seedStart = seedStart + movement,
            seedEnd = seedEnd + movement
          }
      ]
      where
        SeedRange {seedStart, seedEnd} = seedRange
        movement = destStart - srcStart
    mapSeedRange (Just seedRange) Nothing = [seedRange]
    mapSeedRange _ _ = []

    getSplitAfterPosition :: SeedRange -> MapRange -> Int
    getSplitAfterPosition SeedRange {seedStart, seedEnd} MapRange {destStart, srcStart, count} =
      let (mapSrcStart, mapSrcEnd) = (srcStart, srcStart + count -1)
       in getSplitAfterPosition' seedStart seedEnd mapSrcStart mapSrcEnd
      where
        getSplitAfterPosition' seedStart seedEnd mapSrcStart mapSrcEnd
          | seedStart < mapSrcStart = min seedEnd (mapSrcStart -1)
          | seedStart == mapSrcStart = min seedEnd mapSrcEnd
          | seedStart > mapSrcStart = min (seedStart -1) mapSrcEnd
          | otherwise = error "Shouldn't happen."
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Day15 where

import Control.Monad
import Control.Monad.State.Strict
import Data.Char (digitToInt, isAlphaNum, isDigit, isSpace, ord)
import Data.Function (on)
import Data.HashMap.Internal.Array (pair)
import Data.List (dropWhileEnd, elemIndex, elemIndices, find, findIndex, group, groupBy, intercalate, intersect, maximumBy, minimumBy, nub, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (chunksOf, splitEvery, splitOn, splitOneOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as V
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace (trace)

log2 v = trace (show v) v

cases = [Case solveA "Test" 1320, Problem solveA "Problem", Case solveB "Test" 145, Problem solveB "Problem"]

compute :: [Char] -> Int
compute = foldl (\current c -> ((current + ord c) * 17) `mod` 256) 0

solveA :: [String] -> Int
solveA = sum . map compute . splitOn "," . head

data Lens = Lens {lensLabel :: String, lensFocal :: Int}

data Action = Add {addFocal :: Int} | Remove

data Operation = Operation {label :: String, action :: Action}

solveB :: [String] -> Int
solveB = V.sum . V.map scoreBox . V.indexed . V.foldl applyAction (V.replicate 256 []) . V.fromList . map readAction . splitOn "," . head
  where
    readAction str = case reverse str of
      focal : '=' : labelRev -> Operation {label = reverse labelRev, action = Add {addFocal = digitToInt focal}}
      '-' : labelRev -> Operation {label = reverse labelRev, action = Remove}
      _ -> error "Unexpected input."

    applyAction :: Vector [Lens] -> Operation -> Vector [Lens]
    applyAction boxes operation@Operation {label} =
      let boxNum = compute label
       in boxes V.// [(boxNum, updateBox (boxes V.! boxNum) operation)]

    updateBox :: [Lens] -> Operation -> [Lens]
    updateBox lenses Operation {label, action = Add {addFocal}} = case findIndex (\Lens {lensLabel} -> lensLabel == label) lenses of
      Nothing -> Lens {lensLabel = label, lensFocal = addFocal} : lenses
      Just idx -> take idx lenses ++ [Lens {lensLabel = label, lensFocal = addFocal}] ++ drop (idx + 1) lenses
    updateBox lenses Operation {label, action = Remove} = case findIndex (\Lens {lensLabel} -> lensLabel == label) lenses of
      Nothing -> lenses
      Just idx -> take idx lenses ++ drop (idx + 1) lenses

    scoreBox :: (Int, [Lens]) -> Int
    scoreBox (boxIdx, lenses) = (* (boxIdx + 1)) . sum . zipWith (*) [1 ..] . map lensFocal . reverse $lenses
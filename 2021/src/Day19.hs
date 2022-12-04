{-# LANGUAGE TupleSections #-}

module Day19 where

import Control.Applicative
import Data.Char (digitToInt, isNumber)
import Data.Complex (magnitude)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, group, groupBy, intercalate, isInfixOf, nub, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)
import GHC.Base (join)
import Numeric (readHex)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

cases =
  [ Case solveA "Test" 79,
    Problem solveA "Problem",
    Case solveB "Test" 3621,
    Problem solveB "Problem"
  ]

trace' :: Show a => a -> a
trace' value = trace (show value) value

trace'' :: Show a => String -> a -> a
trace'' msg value = trace (msg ++ " " ++ show value) value

showBeacons :: [Beacon] -> [Char]
showBeacons = intercalate "\n" . map ((" " ++) . show . coord)

traceBeacons :: String -> [Beacon] -> [Beacon]
traceBeacons msg beacons = trace (msg ++ "\n" ++ showBeacons beacons) beacons

traceState :: (Solution, Prob) -> (Solution, Prob)
traceState (Solution knownBeacons, Prob scanners) =
  trace
    ( show (length scanners) ++ " scanners. "
        ++ show (length knownBeacons)
        ++ " known beacons:\n"
        ++ showBeacons knownBeacons
    )
    (Solution knownBeacons, Prob scanners)

inspect :: Show a => a -> Int
inspect value = trace (show value) 0

solveA :: [String] -> Int
solveA lines =
  let problem = readProblem lines
      (firstScanner, remainingProblems) = takeScanner 0 problem
      initialSolution = Solution (getBeacons firstScanner)
      nullScanners (_, Prob scanners) = null scanners
      (solution, nullProblems) = until nullScanners iterateState (initialSolution, remainingProblems)
   in (length . knownBeacons) solution

iterateState :: (Solution, Prob) -> (Solution, Prob)
iterateState (Solution knownBeacons, Prob scanners) =
  let (newBeacons, remainingScanners) = locateNextScanner knownBeacons scanners
   in (Solution ((nub . sort) (knownBeacons ++ newBeacons)), Prob remainingScanners)

locateNextScanner :: [Beacon] -> [Scanner] -> ([Beacon], [Scanner])
locateNextScanner knownBeacons scanners =
  case takeFirstJust (tryMatchScanner knownBeacons) scanners of
    Just (matchedBeacons, remainingScanners) -> (matchedBeacons, remainingScanners)
    Nothing -> error "Failed to find next scanner"

tryMatchScanner :: [Beacon] -> Scanner -> Maybe [Beacon]
tryMatchScanner knownBeacons (Scanner scannerBeacons) =
  case takeFirstJust (tryMatchBeacons knownBeacons) (getRotations scannerBeacons) of
    Just (matchedBeacons, _) -> Just matchedBeacons
    Nothing -> Nothing

tryMatchBeacons :: [Beacon] -> [Beacon] -> Maybe [Beacon]
tryMatchBeacons knownBeacons candidateBeacons =
  case tryFindDelta (map coord knownBeacons) (map coord candidateBeacons) of
    Just delta -> Just (map (moveBeacon delta) candidateBeacons)
    Nothing -> Nothing

tryFindDelta :: [Coord] -> [Coord] -> Maybe Coord
tryFindDelta as bs =
  let allPairDiffs = map diffCoordPair $ combos as bs
      g = (group . sort) allPairDiffs
      mostCommonDiff = last $ sortByF length g
   in if length mostCommonDiff >= 12
        then Just (head mostCommonDiff)
        else Nothing

takeFirstJust :: (item -> Maybe result) -> [item] -> Maybe (result, [item])
takeFirstJust check items =
  let takeFirstJust' check skippedItems (item : nextItems) =
        case check item of
          Just result -> Just (result, skippedItems ++ nextItems)
          Nothing -> takeFirstJust' check (item : skippedItems) nextItems
      takeFirstJust' check skippedItems [] = Nothing
   in takeFirstJust' check [] items

combos :: [a] -> [a] -> [(a, a)]
combos as bs = [(x, y) | x <- as, y <- bs]

diffCoordPair :: (Coord, Coord) -> Coord
diffCoordPair ((x1, y1, z1), (x2, y2, z2)) = (x1 - x2, y1 - y2, z1 - z2)

sortByF :: Ord a => (t -> a) -> [t] -> [t]
sortByF f = sortBy (\x y -> compare (f x) (f y))

moveBeacon :: Coord -> Beacon -> Beacon
moveBeacon (dx, dy, dz) (Beacon (x, y, z)) = Beacon (x + dx, y + dy, z + dz)

getRotations :: [Beacon] -> [[Beacon]]
getRotations bs =
  let rotX (Beacon (x, y, z)) = Beacon (x, - z, y)
      rotY (Beacon (x, y, z)) = Beacon (- z, y, x)
      rotZ (Beacon (x, y, z)) = Beacon (- y, x, z)
      four f = [f, f . f, f . f . f, f . f . f . f]
      rotators =
        map (uncurry (.)) (combos (four rotX) (four rotY))
          ++ map (. rotZ) (four rotX)
          ++ map (. rotZ . rotZ . rotZ) (four rotX)
      rotate r bs = map (Beacon . r . coord) bs
   in map (`map` bs) rotators

type Coord = (Int, Int, Int)

data Beacon = Beacon {coord :: Coord} deriving (Eq, Ord)

instance Show Beacon where
  show (Beacon (x, y, z)) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

data Scanner = Scanner {getBeacons :: [Beacon]} deriving (Show)

data Solution = Solution {knownBeacons :: [Beacon]}

instance Show Solution where
  show (Solution bs) = intercalate "\n" $ map show $ sort bs

addBeacons :: Solution -> [Beacon] -> Solution
addBeacons (Solution bs) newBeacons = Solution (nub (newBeacons ++ bs))

data Prob = Prob [Scanner] deriving (Show)

takeScanner :: Int -> Prob -> (Scanner, Prob)
takeScanner i (Prob xs) =
  let s = xs !! i
      remaining = take i xs ++ drop (i + 1) xs
   in (s, Prob remaining)

readBeacon :: String -> Beacon
readBeacon =
  let readToArray = map read . splitOn ","
   in (\[x, y, z] -> Beacon (x, y, z)) . readToArray

readProblem :: [String] -> Prob
readProblem =
  Prob
    . map
      ( Scanner
          . map readBeacon
          . filter (not . null)
      )
    . groupBy (\_ b -> not $ null b)
    . filter (not . isInfixOf "scanner")

solveB :: [String] -> Int
solveB lines = -1

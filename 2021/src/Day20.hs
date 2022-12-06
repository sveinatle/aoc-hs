module Day20 where

import Control.Applicative
import Data.Char (digitToInt, isNumber)
import Data.Complex (magnitude)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, group, groupBy, intercalate, isInfixOf, maximumBy, nub, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (chunksOf, splitEvery, splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Tuple (swap)
import Data.Vector (Vector, fromList, toList, (!), (!?))
import qualified Data.Vector as V
import DayProblem (Case (Case, Problem))
import Debug.Trace (trace)
import GHC.Base (join)
import Numeric (readHex)
import Text.Printf (printf)
import Text.Regex (matchRegex, mkRegex)

cases =
  [ Case solveA "Test" 35,
    Problem solveA "Problem",
    Case solveB "Test" 3351,
    Problem solveB "Problem"
  ]

trace' :: Show a => a -> a
trace' value = trace (show value) value

trace'' :: Show a => String -> a -> a
trace'' msg value = trace (msg ++ " " ++ show value) value

inspect value = trace (show value) 0

readProblem :: Int -> [String] -> (Int, Int, Vector Bool, Vector Bool)
readProblem padding lines =
  let algo = fromList $ map (== '#') (head lines)
      imageLines = drop 2 lines
      padLine line = replicate padding '.' ++ line ++ replicate padding '.'
      width = length (head imageLines) + 2 * padding
      paddedImage =
        V.concat $
          concat
            [ replicate padding (V.replicate width '.'),
              map (fromList . padLine) imageLines,
              replicate padding (V.replicate width '.')
            ]
      image = V.map (== '#') paddedImage
   in (width, length paddedImage `div` width, algo, image)

solveA :: [String] -> Int
solveA lines =
  let (width, height, algo, image) = readProblem 2 lines
      processor = processImage width height algo
      output = (processor . processor) image
   in (length . V.filter (== True)) output

processImage :: Int -> Int -> Vector Bool -> Vector Bool -> Vector Bool
processImage width height algo input = V.generate (width * height) (processPixel width height algo input)

showImage :: Int -> Vector Bool -> Vector Bool
showImage width image =
  let imageLines = intercalate "\n" $ chunksOf width (toList $ V.map (\v -> if v then '#' else '.') image)
   in trace imageLines image

processPixel :: Int -> Int -> Vector Bool -> Vector Bool -> Int -> Bool
processPixel width height algo input idx =
  let p = getPixelValue width height input
      x = idx `mod` width
      y = idx `div` width
      neighbourhood =
        reverse
          [ p (x - 1) (y - 1),
            p x (y - 1),
            p (x + 1) (y - 1),
            --
            p (x - 1) y,
            p x y,
            p (x + 1) y,
            --
            p (x - 1) (y + 1),
            p x (y + 1),
            p (x + 1) (y + 1)
          ]
      algoIdx =
        --trace'' (show (x, y, neighbourhood)) $
        V.sum $
          V.map (uncurry (*)) $
            V.zip (V.generate 9 (2 ^)) (fromList neighbourhood)
   in algo V.! algoIdx

getPixelValue :: Int -> Int -> Vector Bool -> Int -> Int -> Int
getPixelValue width height image x y =
  let clampedX = max 0 (min (width -1) x)
      clampedY = max 0 (min (height -1) y)
   in if image ! (clampedY * width + clampedX) then 1 else 0

solveB :: [String] -> Int
solveB lines =
  let (width, height, algo, image) = readProblem 50 lines
      processor = processImage width height algo
      output = iterate processor image !! 50
   in (length . V.filter (== True)) output
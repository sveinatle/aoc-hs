module Day03 where

import Data.List (transpose)
import DayProblem

cases = [Case solveA "Test" 198, Problem solveA "Problem", Case solveB "Test" 230, Problem solveB "Problem"]

split2PlusMinus :: [Char] -> [Int]
split2PlusMinus = map (\c -> if c == '1' then 1 else -1)

split2Bool :: [Char] -> [Bool]
split2Bool = map (== '1')

bools2int :: [Bool] -> Int
bools2int bools =
  let boolsRev = reverse bools
      bools2int' [] = 0
      bools2int' (b : bs) = (if b then 1 else 0) + 2 * bools2int' bs
   in bools2int' boolsRev

signed2int :: [Int] -> Int
signed2int signed =
  let signedRev = reverse signed
      signed2int' [] = 0
      signed2int' (b : bs) = (if b > 0 then 1 else 0) + 2 * signed2int' bs
   in signed2int' signedRev

solveA :: [String] -> Int
solveA lines =
  let plusMinusArrayPerBit = transpose $map split2PlusMinus lines
      bitSums = map sum plusMinusArrayPerBit
      gammaBools = map (> 0) bitSums
      gamma = bools2int gammaBools
      epsilon = bools2int $ map not gammaBools
   in gamma * epsilon

defaultValue :: Int -> Int -> Int
defaultValue def v = if v == 0 then def else v

mostCommon :: [[Int]] -> [Int]
mostCommon bitArrays
  | null (concat bitArrays) = []
  | otherwise =
    let firstBits = map head bitArrays
        mostCommonBit = defaultValue 1 $(signum . sum) firstBits
        matchingBitArrays = filter (\bitArray -> head bitArray == mostCommonBit) bitArrays
     in (mostCommonBit : mostCommon (map tail matchingBitArrays))

leastCommon :: [[Int]] -> [Int]
leastCommon [singleArray] = singleArray -- If single candidate left, then just return the rest of it.
leastCommon bitArrays
  | null (concat bitArrays) = []
  | otherwise =
    let firstBits = map head bitArrays
        mostCommonBit = (signum . sum) firstBits
        leastCommonBit = defaultValue (-1) (- mostCommonBit)
        matchingBitArrays = filter (\bitArray -> head bitArray == leastCommonBit) bitArrays
     in (leastCommonBit : leastCommon (map tail matchingBitArrays))

solveB :: [String] -> Int
solveB lines =
  let bitArrays = map split2PlusMinus lines
      oxygen = mostCommon bitArrays
      co2 = leastCommon bitArrays
   in signed2int oxygen * signed2int co2
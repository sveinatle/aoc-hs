module Day14 where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import DayProblem
import Debug.Trace (trace)

cases = [Case solveA "Test" 1588, Problem solveA "Problem", Case solveB "Test" 2188189693529, Problem solveB "Problem"]

solveA :: [String] -> Int
solveA lines = solve lines 10

solveB :: [String] -> Int
solveB lines = solve lines 40

trace' :: Show a => a -> a
trace' value = trace (show value) value

readRule :: [Char] -> Rule
readRule line =
  let [[a, b], [to]] = splitOn " -> " line
   in Rule (a, b) to

type Pair = (Char, Char)

type Steps = Int

data Rule = Rule
  { rulePairField :: Pair,
    ruleInjectField :: Char
  }
  deriving (Eq, Ord)

type CharacterCounts = Map Char Int

data CacheKey = CacheKey Pair Steps deriving (Eq, Ord, Show)

type Cache = Map CacheKey CharacterCounts

ruleToCharacterCounts :: Rule -> CharacterCounts
ruleToCharacterCounts Rule {rulePairField = (left, right), ruleInjectField = inject} = Map.fromListWith (+) [(left, 1), (right, 1), (inject, 1)]

pairToCharacterCounts :: Pair -> CharacterCounts
pairToCharacterCounts (left, right) = Map.fromListWith (+) [(left, 1), (right, 1)]

decCharacterCount :: Char -> CharacterCounts -> CharacterCounts
decCharacterCount = Map.update (Just . subtract 1)

solve :: [String] -> Steps -> Int
solve lines steps =
  let initial = head lines
      topLevelPairs = zip initial (tail initial)
      rules = map readRule $ (tail . tail) lines
      initialCache = Map.fromList $ map (\r -> (CacheKey (rulePairField r) 0, ruleToCharacterCounts r)) rules
      cache = foldr (updateCache steps rules) initialCache topLevelPairs
      getPairCharCounts pair = cache Map.! CacheKey pair steps
      charCountsPerPair = map getPairCharCounts topLevelPairs
      charCountsSummed = foldl1 (Map.unionWith (+)) charCountsPerPair
      charCountsMerged = foldr decCharacterCount charCountsSummed ((tail . init) initial) -- Decrease "internal" characters by one since they are part of two pairs each.
      charCounts = Map.elems charCountsMerged
   in maximum charCounts - minimum charCounts

updateCache :: Steps -> [Rule] -> Pair -> Cache -> Cache
updateCache 0 rules pair cache =
  -- Won't be split any more, so add entry for this final pair.
  Map.insert (CacheKey pair 0) (pairToCharacterCounts pair) cache
updateCache steps rules pair cache =
  -- First check if the pair + step count is already cached.
  case Map.lookup (CacheKey pair steps) cache of
    Just _ ->
      -- Return unmodified cache if it already contains an appropriate entry.
      cache
    Nothing ->
      -- Add entry for pair + step count.
      case find ((== pair) . rulePairField) rules of
        Just Rule {ruleInjectField = inject} ->
          -- Found rule for pair. Split pair and update cache with sub-pairs.
          updateCacheWithSplitPair steps rules (fst pair, inject, snd pair) cache
        Nothing ->
          -- No rule found. Pair won't be split so add entry for just this pair.
          Map.insert (CacheKey pair steps) (pairToCharacterCounts pair) cache

updateCacheWithSplitPair :: Steps -> [Rule] -> (Char, Char, Char) -> Map CacheKey CharacterCounts -> Map CacheKey CharacterCounts
updateCacheWithSplitPair steps rules (left, inject, right) cache =
  let remainingSteps = steps - 1
      leftPair = (left, inject)
      rightPair = (inject, right)
      leftPairCache = updateCache remainingSteps rules leftPair cache
      rightPairCache = updateCache remainingSteps rules rightPair leftPairCache
      leftPairCharacterCounts = leftPairCache Map.! CacheKey leftPair remainingSteps
      rightPairCharacterCounts = rightPairCache Map.! CacheKey rightPair remainingSteps
      summedCharacterCounts = Map.unionWith (+) leftPairCharacterCounts rightPairCharacterCounts
      mergedCharacterCounts = decCharacterCount inject summedCharacterCounts -- The injected character is counted in both sub-pairs, so subtract 1 from its count in the merged map.
   in Map.insert (CacheKey (left, right) steps) mergedCharacterCounts rightPairCache

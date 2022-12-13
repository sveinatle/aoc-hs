module Day13 where

import Data.Char
import Data.Foldable (Foldable (fold, foldr'))
import Data.Ix (Ix (inRange))
import Data.List
import Data.List.Split (chunksOf, splitOn, splitWhen)
import Data.String (IsString)
import DayProblem (Case (Case, CaseStr, Problem, ProblemStr))
import Debug.Trace
import GHC.Real (reduce)
import Text.Regex (matchRegex, mkRegex)

t v = trace (show v) v

t0 v = trace (show v) 0

t' txt v = trace (txt ++ show v) v

cases =
  [ Case solveA "Test" 13,
    Problem solveA "Problem",
    Case solveB "Test" 0,
    Problem solveB "Problem"
  ]

data Packet = List [Packet] | Num Int

instance Show Packet where
  show (Num n) = show n
  show (List l) = show l

tracePair :: (Show a, Show b) => (a, b) -> (a, b)
tracePair (p1, p2) = trace (show p1 ++ "\n" ++ show p2 ++ "\n") (p1, p2)

readListItems :: String -> ([Packet], String)
readListItems (']' : str) = ([], ']' : str)
readListItems (',' : str) = readListItems str
readListItems str =
  let (packet, remainingStr) = readPacket str
      (packets, remainingStr') = readListItems remainingStr
   in (packet : packets, remainingStr')

readListPacket :: String -> (Packet, String)
readListPacket str =
  case readListItems str of
    (listItems, ']' : remainingStr) -> (List listItems, remainingStr)
    (listItems, _) -> error "Expected list close."

readIntPacket :: String -> (Packet, String)
readIntPacket str =
  let (intStr, remainingStr) = span isDigit str
   in (Num (read intStr), remainingStr)

readPacket :: String -> (Packet, String)
readPacket ('[' : str) = readListPacket str
readPacket str = readIntPacket str

readPair :: [String] -> (Packet, Packet)
readPair [p1, p2] = (fst $ readPacket p1, fst $ readPacket p2)
readPair _ = error "Unexpected input."

comparePackets :: Packet -> Packet -> Ordering
comparePackets (Num n1) (Num n2) = compare n1 n2
comparePackets (Num n1) (List l2) = comparePackets (List [Num n1]) (List l2)
comparePackets (List l1) (Num n2) = comparePackets (List l1) (List [Num n2])
comparePackets (List []) (List []) = EQ
comparePackets (List []) (List _) = LT
comparePackets (List _) (List []) = GT
comparePackets (List l1) (List l2) = case comparePackets (head l1) (head l2) of
  EQ -> comparePackets (List (tail l1)) (List (tail l2))
  r -> r

solveA :: [String] -> Int
solveA = sum . map (+ 1) . elemIndices True . map (not . (==) GT . uncurry comparePackets . readPair) . splitOn [""]

solveB :: [String] -> Int
solveB lines = 0

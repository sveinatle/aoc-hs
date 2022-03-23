module Day16 where

import Data.Char (digitToInt)
import Data.Foldable (minimumBy)
import Data.List (delete, find, findIndex, permutations, sort, sortBy, transpose, (\\))
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as MinQueue
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import DayProblem
import Debug.Trace (trace)
import Numeric (readHex)
import Text.Printf (printf)

cases =
  [ Case solveA "Test1" 16,
    Case solveA "Test2" 12,
    Case solveA "Test3" 23,
    Case solveA "Test4" 31,
    Problem solveA "Problem",
    Case solveB "TestB1" 3,
    Case solveB "TestB2" 54,
    Case solveB "TestB3" 7,
    Case solveB "TestB4" 9,
    Case solveB "TestB5" 1,
    Case solveB "TestB6" 0,
    Case solveB "TestB7" 0,
    Case solveB "TestB8" 1,
    Problem solveB "Problem"
  ]

solveA :: [String] -> Int
solveA lines =
  let packet = readPacketHex $head lines
   in readVersionsSum packet

solveB :: [String] -> Int
solveB lines =
  let packet = readPacketHex $head lines
   in evaluatePacket packet

trace' :: Show a => a -> a
trace' value = trace (show value) value

type Bits = [Int]

hexToBits :: Char -> Bits
hexToBits c =
  case readHex [c] of
    (x, _) : _ -> map (\b -> if b == '1' then 1 else 0) $ printf "%04b" (x :: Int)
    _ -> error "Invalid input."

bitsToInt :: Bits -> Int
bitsToInt bits =
  let bitsToInt' [] v = v
      bitsToInt' (b : bs) v = bitsToInt' bs (v * 2 + b)
   in bitsToInt' bits 0

readSlice :: Int -> (Bits -> a) -> Bits -> (a, Bits)
readSlice idx convert xs = (convert $take idx xs, drop idx xs)

readVersionsSum :: Packet -> Int
readVersionsSum (Packet version typ (Literal _)) = version
readVersionsSum (Packet version typ (FixedSizeOperator subPackets)) = version + sum (map readVersionsSum subPackets)
readVersionsSum (Packet version typ (ItemCountOperator subPackets)) = version + sum (map readVersionsSum subPackets)

evaluatePacket :: Packet -> Int
evaluatePacket (Packet version typ (Literal v)) = v
evaluatePacket (Packet version typ (FixedSizeOperator subPackets)) = evaluateOperatorPacket typ (map evaluatePacket subPackets)
evaluatePacket (Packet version typ (ItemCountOperator subPackets)) = evaluateOperatorPacket typ (map evaluatePacket subPackets)

evaluateOperatorPacket :: Int -> [Int] -> Int
evaluateOperatorPacket 0 subPacketValues = sum subPacketValues
evaluateOperatorPacket 1 subPacketValues = product subPacketValues
evaluateOperatorPacket 2 subPacketValues = minimum subPacketValues
evaluateOperatorPacket 3 subPacketValues = maximum subPacketValues
evaluateOperatorPacket 5 [p1, p2] = if p1 > p2 then 1 else 0
evaluateOperatorPacket 6 [p1, p2] = if p1 < p2 then 1 else 0
evaluateOperatorPacket 7 [p1, p2] = if p1 == p2 then 1 else 0
evaluateOperatorPacket _ _ = error "Unexpected operator packet content."

readPacketHex :: String -> Packet
readPacketHex line =
  let bits = concatMap hexToBits line
      (packet, restBits) = readPacket bits
   in packet

data Packet = Packet {pVersion :: Int, pType :: Int, pContent :: PacketContent} deriving (Show)

data PacketContent
  = Literal {value :: Int}
  | FixedSizeOperator {packets :: [Packet]}
  | ItemCountOperator {packets :: [Packet]}
  deriving (Show)

readPacket :: Bits -> (Packet, Bits)
readPacket bits =
  let (version, bits') = readSlice 3 bitsToInt bits
      (typ, bits'') = readSlice 3 bitsToInt bits'
      (content, restBits) = case typ of
        -- Literal packet
        4 -> readLiteral bits''
        -- Operator packet
        _ -> readOperator bits''
   in (Packet version typ content, restBits)

readVarInt :: Bits -> ([Int], Bits)
readVarInt (b : bits) =
  let (dataBits, restBits) = readSlice 4 id bits
      mergeDataBits dataBitsMsb (dataBitsLsb, restBits) = (dataBitsMsb ++ dataBitsLsb, restBits)
   in case b of
        1 -> mergeDataBits dataBits (readVarInt restBits)
        0 -> (dataBits, restBits)
        _ -> error "Expected 1 or 0."
readVarInt [] = error "Not enough bits."

readLiteral :: Bits -> (PacketContent, Bits)
readLiteral bits =
  let (literalBits, restBits) = readVarInt bits
   in (Literal (bitsToInt literalBits), restBits)

readOperator :: Bits -> (PacketContent, Bits)
readOperator [] = error "Not enough bits."
readOperator (lengthTypeBit : bits) = case lengthTypeBit of
  0 -> readFixedSizeOperator bits
  1 -> readItemCountOperator bits
  _ -> error "Expected 1 or 0."

readAndMergePacketOutput :: ([Packet], Bits) -> ([Packet], Bits)
readAndMergePacketOutput (packets, bits) =
  let (newPacket, restBits) = readPacket bits
   in (newPacket : packets, restBits)

readFixedSizeOperator :: [Int] -> (PacketContent, Bits)
readFixedSizeOperator bits =
  let (bitLength, bits') = readSlice 15 bitsToInt bits
      (subBits, restBits) = readSlice bitLength id bits'
      packetsBackwards = fst $ until (null . snd) readAndMergePacketOutput ([], subBits)
   in (FixedSizeOperator (reverse packetsBackwards), restBits)

readItemCountOperator :: [Int] -> (PacketContent, Bits)
readItemCountOperator bits =
  let (itemCount, bits') = readSlice 11 bitsToInt bits
      (packetsBackwards, restBits) = iterate readAndMergePacketOutput ([], bits') !! itemCount
   in (ItemCountOperator (reverse packetsBackwards), restBits)
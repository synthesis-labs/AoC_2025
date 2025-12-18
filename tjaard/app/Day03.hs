module Day03 where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

solve :: IO ()
solve = do
  input <- readFile "inputs/day03"
  let batteryBanks = map (map digitToInt) (lines input)
  putStrLn $ "Part 1: " ++ show (calculateJoltage batteryBanks 2)
  putStrLn $ "Part 2: " ++ show (calculateJoltage batteryBanks 12)

calculateJoltage :: [[Int]] -> Int -> Int
calculateJoltage batteryBanks n = sum $ map (read . concatMap show . joltage n) batteryBanks

joltage :: Int -> [Int] -> [Int]
joltage 1 batteries = [maximum batteries]
joltage n batteries = maxVal : joltage (n - 1) (drop (maxIdx + 1) batteries)
  where
    window = take (length batteries - n + 1) batteries
    maxVal = maximum window
    maxIdx = fromJust $ elemIndex maxVal batteries
module Day05 where

import Data.List.Split (splitOn)
import Data.List (sort)

solve :: IO ()
solve = do
  input <- readFile "inputs/day05"
  let inventory = parse (lines input)
  putStrLn $ "Part 1: " ++ show (part1 inventory)
  putStrLn $ "Part 2: " ++ show (part2 inventory)

-- Definitions
data Inventory = Inventory
  { freshRanges :: [(Int, Int)],
    ingredients :: [Int]
  }

parse :: [String] -> Inventory
parse ls = Inventory (parseRange <$> ranges) (read <$> tail rest)
  where
    (ranges, rest) = span (not . null) ls
    parseRange s = case splitOn "-" s of
      [a, b] -> (read a, read b)
      _ -> error "Invalid range format"

part1 :: Inventory -> Int
part1 inventory =
  length $ filter (`isFresh` freshRanges inventory) (ingredients inventory)

part2 :: Inventory -> Int
part2 inventory = sum $ map rangeSize merged
  where
    merged = mergeRanges . sort $ freshRanges inventory
    rangeSize (start, end) = end - start + 1

isFresh :: Int -> [(Int, Int)] -> Bool
isFresh ingredient ranges = any (\(start, end) -> ingredient >= start && ingredient <= end) ranges

mergeRanges :: [(Int, Int)] -> [(Int, Int)]
mergeRanges [] = []
mergeRanges (r:rs) = foldl merge [r] rs
  where
    merge acc@((a1, z1):rest) (a2, z2)
      | z1 >= a2 - 1 = (a1, max z1 z2) : rest
      | otherwise = (a2, z2) : acc
    merge [] range = [range]
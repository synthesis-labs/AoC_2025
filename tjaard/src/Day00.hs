module Day00 where

import Data.List (sort)

solve :: IO ()
solve = do
  input <- readFile "inputs/day00-test"
  let (left, right) = parse input
  putStrLn $ "Part 1: " ++ show (part1 left right)
  putStrLn $ "Part 2: " ++ show (part2 left right)

parse :: String -> ([Int], [Int])
parse input = unzip [(read a, read b) | line <- lines input, [a, b] <- [words line]]

part1 :: [Int] -> [Int] -> Int
part1 left right = totalDifference (sort left) (sort right)

part2 :: [Int] -> [Int] -> Int
part2 = occurenceTotal

totalDifference :: [Int] -> [Int] -> Int
totalDifference xs ys = sum $ zipWith (\x y -> abs (x - y)) xs ys

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x = length . filter (== x)

occurenceTotal :: [Int] -> [Int] -> Int
occurenceTotal xs ys = sum [x * countOccurrences x ys | x <- xs]
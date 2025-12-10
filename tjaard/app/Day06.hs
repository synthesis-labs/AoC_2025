module Day06 where

import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Split (splitWhen)

solve :: IO ()
solve = do
  input <- readFile "inputs/day06-test"
  putStrLn $ "Part 1: " ++ show (sum $ solveProblem <$> parse input)
  putStrLn $ "Part 2: " ++ show (sum $ solveProblem <$>parse' input)

-- Definitions
type Operator = String
type Terms = [Int]
type Problem = (Operator, Terms)

parse :: String -> [Problem]
parse input = map toProblem problemStrings
  where
    problemStrings = transpose (map words . lines $ input) :: [[String]]
    toProblem p = (last p, map read . init $ p) :: Problem

parse' :: String -> [Problem]
parse' input = map toProblem columns
  where
    columns = splitWhen (all isSpace) . transpose . lines $ input
    toProblem col = (operator col, map number col)
    operator = filter (`elem` ['+', '*']) . concat
    number = read . filter isDigit    

solveProblem :: Problem -> Int
solveProblem ("+", terms) = sum terms
solveProblem ("*", terms) = product terms
solveProblem _ = 0
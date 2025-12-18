module Day02 where

import Data.List.Split (splitOn, chunksOf)

solve :: IO ()
solve = do
  input <- readFile "inputs/day02"
  let ids = parse input
  putStrLn $ "Part 1: " ++ show (part1 ids)
  putStrLn $ "Part 2: " ++ show (part2 ids)

-- Definitions
type ID = Int

parse :: String -> [ID]
parse = concatMap expandRange . splitOn ","
  where
    expandRange range = case splitOn "-" range of
      [start, end] -> [read start .. read end]
      _ -> error "Expected range format: start-end"

part1 :: [ID] -> Int
part1 = sum . filter halvesSame

part2 :: [ID] -> Int
part2 ids = sum [id' | id' <- ids, any (allChunksSame id') [1..digits id']]

digits :: ID -> Int
digits = length . show

halvesSame :: ID -> Bool
halvesSame id' = fst split == snd split
  where 
    s = show id'
    split = splitAt (digits id' `div` 2) s

allChunksSame :: ID -> Int -> Bool
allChunksSame id' n = case chunksOf n (show id') of
  (x:y:rest) -> all (== x) (y:rest)
  _ -> False

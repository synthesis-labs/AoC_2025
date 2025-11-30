module Day01 where

solve :: IO ()
solve = do
  input <- readFile "inputs/day01-test"
  let parsed = parse input
  putStrLn $ "Part 1: " ++ show (part1 parsed)
  putStrLn $ "Part 2: " ++ show (part2 parsed)

parse :: String -> [String]
parse = lines

part1 :: [String] -> Int
part1 _ = 0  -- TODO: implement

part2 :: [String] -> Int
part2 _ = 0  -- TODO: implement

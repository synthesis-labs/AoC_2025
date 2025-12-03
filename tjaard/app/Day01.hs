module Day01 where

solve :: IO ()
solve = do
  input <- readFile "inputs/day01"
  let parsed = parse input
  putStrLn $ "Part 1: " ++ show (part1 parsed)
  putStrLn $ "Part 2: " ++ show (part2 parsed)

-- Definitions
type Position = Int
type Turn = Int -- negative for left, positive for right
type DialSize = Int
type DialPosition = Int
dialSize :: DialSize
dialSize = 100
dialStart :: DialPosition
dialStart = 50

parse :: String -> [Turn]
parse = map parseTurn . lines
  where
    parseTurn ('L' : xs) = -(read xs) -- negative for left
    parseTurn ('R' : xs) = read xs -- positive for right
    parseTurn _ = error "Invalid turn format"

turn :: Position -> Turn -> Position
turn pos dist = (pos + dist) `mod` dialSize

distanceToZero :: DialSize -> DialPosition -> Turn -> Int
distanceToZero n p s
  | s >= 0 = p
  | otherwise = (n - p) `mod` n

countZeroClicks :: DialSize -> DialPosition -> Turn -> Int
countZeroClicks n p s = (distanceToZero n p s + abs s) `div` n

part1 :: [Turn] -> Int
part1 turns = length . filter (== 0) $ scanl turn dialStart turns

part2 :: [Turn] -> Int
part2 turns = sum $ zipWith (countZeroClicks dialSize) positions turns
  where
    positions = scanl turn dialStart turns
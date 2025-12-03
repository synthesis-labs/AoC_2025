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
countZeroClicks n p s =
  (distanceToZero n p s + abs s) `div` n

countWithTurns :: (Position -> Turn -> Int) -> [Turn] -> Int
countWithTurns f turns = snd $ foldl step (dialStart, 0) turns
  where
    step (pos, count) dist = (turn pos dist, count + f pos dist)

part1 :: [Turn] -> Int
part1 = countWithTurns (\pos dist -> if turn pos dist == 0 then 1 else 0)

part2 :: [Turn] -> Int
part2 = countWithTurns (countZeroClicks dialSize)
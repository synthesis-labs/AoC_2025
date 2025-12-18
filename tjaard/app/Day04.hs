module Day04 where

import CharGrid

solve :: IO ()
solve = do
  input <- readFile "inputs/day04"
  let grid = fromLines (lines input)
  putStrLn $ "Part 1: " ++ show (length $ accessableCoords grid)
  putStrLn $ "Part 2: " ++ show (countAndRemove grid)

accessable :: CharGrid -> Coord -> Bool
accessable grid c = isRoll c && adjacentRolls c < 4
  where
    adjacentRolls coord = length $ filter isRoll $ neighbors grid coord

accessableCoords :: CharGrid -> [Coord]
accessableCoords grid = [c | c <- coords grid, accessable grid c]

countAndRemove :: CharGrid -> Int
countAndRemove grid = case accessableCoords grid of
  [] -> 0
  cs -> length cs + countAndRemove (removeRolls grid cs)

removeRolls :: CharGrid -> [Coord] -> CharGrid
removeRolls grid toRemove = 
  let updates = [((x c, y c), 'x') | c <- toRemove]
  in insertMany updates grid

isRoll :: Coord -> Bool
isRoll (Coord _ _ v) = v == '@'

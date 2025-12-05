module AOC2025.Day04
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord, neighbors8)
import Util.GridUtils.Grid (GridInfo, findValByCoord, inBounds, parseGrid, updateAtCoord)

part1 :: T.Text -> Int
part1 input = length $ V.foldr (findReachableRolls gi) [] grid
  where
    gi@(grid, _, _) = parseGrid id input

part2 :: T.Text -> Int
part2 input = length $ findAllRemovableRolls gi []
  where
    gi = parseGrid id input

findReachableRolls :: GridInfo Char -> (Coord, Char) -> [Coord] -> [Coord]
findReachableRolls gi (coord, val) acc
  | val == '.' = acc
  | surroundingRollsCount < 4 = coord : acc
  | otherwise = acc
  where
    inBoundNeighbors = filter (inBounds gi) $ neighbors8 coord
    addIfNeighbourRoll n c = if findValByCoord gi n == '@' then c + 1 else c
    surroundingRollsCount = foldr addIfNeighbourRoll (0 :: Int) inBoundNeighbors

findAllRemovableRolls :: GridInfo Char -> [Coord] -> [Coord]
findAllRemovableRolls gi@(g, rLen, cLen) acc =
  if null removableRolls
    then acc
    else findAllRemovableRolls (newGrid, rLen, cLen) (acc ++ removableRolls)
  where
    removableRolls = V.foldr (findReachableRolls gi) [] g
    newGrid = foldr (\c gr -> updateAtCoord (gr, rLen, cLen) (c, '.')) g removableRolls

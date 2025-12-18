module AOC2025.Day07
  ( part1,
    part2,
  )
where

import Data.List (nub)
import Data.Text qualified as T
import Data.Vector qualified as V
import Util.GridUtils.Coord (Coord (Coord))
import Util.GridUtils.Grid (GridInfo, findCoordByVal, inBounds, parseGrid)

part1 :: T.Text -> Integer
part1 input = countSplitterHits gi [start] 0
  where
    gi = parseGrid id input
    start = findCoordByVal gi 'S'

part2 :: T.Text -> Integer
part2 input = 0

countSplitterHits :: GridInfo Char -> [Coord] -> Integer -> Integer
countSplitterHits gi@(grid, _, numCols) beamsInRow hits =
  if null (fst splitterCountsForBeamsInRow)
    then hits
    else countSplitterHits gi (fst splitterCountsForBeamsInRow) (hits + snd splitterCountsForBeamsInRow)
  where
    splitterCountsForBeamsInRow = foldr countSplitter ([], 0) beamsInRow
    countSplitter c acc@(newBeams, splitterCount) =
      case nextBeamPos c of
        Nothing -> acc
        Just (nc, nv) ->
          if nv == '^'
            then (nub (siblings nc ++ newBeams), splitterCount + 1)
            else (nc : newBeams, splitterCount)
    nextBeamPos (Coord x y) = grid V.!? ((x + 1) * numCols + y)
    siblings (Coord x y) = filter (inBounds gi) [Coord x (y - 1), Coord x (y + 1)]

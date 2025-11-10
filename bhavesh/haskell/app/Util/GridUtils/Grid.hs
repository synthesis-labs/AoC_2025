module Util.GridUtils.Grid
  ( Grid,
    GridInfo,
    UnboxedGrid,
    parseGrid,
    newGrid,
    showGrid,
    inBounds,
    updateAtCoord,
    findCoordByVal,
    findIdxByCoord,
    findValByCoord,
    findValByIdx,
  )
where

import Data.Bifunctor qualified as BF
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Util.GridUtils.Coord (Coord (Coord))

type Grid a = V.Vector (Coord, a)

type UnboxedGrid a = U.Vector a

type GridInfo a = (Grid a, Int, Int)

showGrid :: (a -> T.Text) -> (Grid a, Int) -> T.Text
showGrid f (grid, rowLen) = foldl foldFn T.empty [0 .. rowLen - 1]
  where
    foldFn acc row = acc <> stringifyRow row <> T.pack "\n"
    stringifyRow row = V.foldl (stringRowFold row) T.empty grid
    stringRowFold row acc (Coord x _, c) = if x == row then acc <> f c else acc

newGrid :: Int -> Int -> a -> Grid a
newGrid rows cols char = V.fromList $ [(Coord x y, char) | x <- [0 .. cols - 1], y <- [0 .. rows - 1]]

parseGrid :: forall a. (Show a) => (Char -> a) -> T.Text -> GridInfo a
parseGrid f input = (grid, numRows, numCols)
  where
    grid = go V.empty 0 . lines $ T.unpack input
    numRows = V.maximum ((\(Coord x _) -> x) . fst <$> grid) + 1
    numCols = V.maximum ((\(Coord _ y) -> y) . fst <$> grid) + 1

    go acc _ [] = acc
    go acc row (x : xs) = go (acc V.++ columns) (row + 1) xs
      where
        columns = V.fromList $ BF.bimap (Coord row) f <$> zip [0 ..] x

updateAtCoord :: GridInfo a -> (Coord, a) -> Grid a
updateAtCoord gi@(grid, _, _) (c, val) =
  V.modify
    (\g -> MV.write g (findIdxByCoord gi c) (c, val))
    grid

findIdxByCoord :: GridInfo a -> Coord -> Int
findIdxByCoord (_, _, rowLen) (Coord x y) = x * rowLen + y

findCoordByVal :: (Eq a) => GridInfo a -> a -> Coord
findCoordByVal (grid, _, _) val = case V.findIndex ((== val) . snd) grid of
  Just i -> fst (grid V.! i)
  Nothing -> error "val not found"

findValByIdx :: GridInfo a -> Int -> a
findValByIdx (grid, _, _) idx = snd $ grid V.! idx

findValByCoord :: GridInfo a -> Coord -> a
findValByCoord gi c = findValByIdx gi $ findIdxByCoord gi c

inBounds :: GridInfo a -> Coord -> Bool
inBounds (_, rows, cols) (Coord x y) = 0 <= x && x < rows && 0 <= y && y < cols

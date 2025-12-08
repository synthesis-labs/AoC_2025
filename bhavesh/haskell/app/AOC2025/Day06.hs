module AOC2025.Day06
  ( part1,
    part2,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (on)
import Data.List (groupBy, transpose, unsnoc)
import Data.Text qualified as T

part1 :: T.Text -> Integer
part1 input = foldr performOp 0 $ zip operations terms
  where
    (terms, operations) =
      maybe
        ([], [])
        (bimap (transpose . (words <$>)) (filter (/= ' ')))
        (unsnoc (lines (T.unpack input)))

part2 :: T.Text -> Integer
part2 input = foldr performOp 0 $ zip operations terms
  where
    isEmpty = all (== ' ')
    groupByEmpties xs = [g | g <- groupBy ((==) `on` isEmpty) xs, not (all isEmpty g)]
    (terms, operations) =
      maybe
        ([], [])
        (bimap (groupByEmpties . transpose) (filter (/= ' ')))
        (unsnoc (lines (T.unpack input)))

performOp :: (Char, [String]) -> Integer -> Integer
performOp (op, l)
  | op == '+' = (+) (sum (convertToNum <$> l))
  | otherwise = (+) (product (convertToNum <$> l))
  where
    convertToNum = read . filter (/= ' ')

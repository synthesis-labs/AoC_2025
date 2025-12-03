module AOC2025.Day03
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.Text qualified as T

part1 :: T.Text -> Integer
part1 input = sum $ maxJolt <$> banks
  where
    banks = zip [0 :: Integer ..] <$> parseBatteries input
    maxJolt bank = maximum $ concatIntList <$> [[x, y] | (i1, x) <- bank, (i2, y) <- bank, i1 < i2]

part2 :: T.Text -> Integer
part2 input = sum $ maxJolt <$> banks
  where
    banks = zip [0 :: Integer ..] <$> parseBatteries input
    maxJolt bank =
      maximum $
        concatIntList
          <$> [ [a, b, c, d, e, f, g, h, i, j, k, l]
              | (i1, a) <- bank,
                (i2, b) <- bank,
                i1 < i2,
                (i3, c) <- bank,
                i2 < i3,
                (i4, d) <- bank,
                i3 < i4,
                (i5, e) <- bank,
                i4 < i5,
                (i6, f) <- bank,
                i5 < i6,
                (i7, g) <- bank,
                i6 < i7,
                (i8, h) <- bank,
                i7 < i8,
                (i9, i) <- bank,
                i8 < i9,
                (i10, j) <- bank,
                i9 < i10,
                (i11, k) <- bank,
                i10 < i11,
                (i12, l) <- bank,
                i11 < i12
              ]

type Battery = [Integer]

concatIntList :: [Integer] -> Integer
concatIntList = foldr1 concatInts

concatInts :: Integer -> Integer -> Integer
concatInts a b = a * 10 ^ length (show (abs b)) + b

parseBatteries :: T.Text -> [Battery]
parseBatteries input = fmap (fromIntegral . digitToInt) . T.unpack <$> T.lines input
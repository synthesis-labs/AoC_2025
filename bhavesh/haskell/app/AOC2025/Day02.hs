module AOC2025.Day02
  ( part1,
    part2,
  )
where

import Data.List (inits, nub, tails)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Integer
part1 input = sum $ concatMap countInvalids ranges
  where
    ranges = parseRanges input
    countInvalids (startN, endN) = filter isInvalid [startN .. endN]

part2 :: T.Text -> Integer
part2 input = sum $ concatMap countInvalids ranges
  where
    ranges = parseRanges input
    countInvalids (startN, endN) = filter isInvalidFast2 [startN .. endN]

isInvalidFast2 :: Integer -> Bool
isInvalidFast2 n = any (\d -> concat (replicate (len `div` d) (take d s)) == s) ds
  where
    s = show n
    len = length s
    ds = [d | d <- [1 .. len `div` 2], len `mod` d == 0]

isInvalid' :: Integer -> Bool
isInvalid' n = any ((== s) . buildRepeatedString s) substrings
  where
    substrings =
      nub $
        concatMap
          (filter (\e -> length e <= l) . filter (not . null) . inits)
          (tails s)
    l = length s `div` 2
    s = show n
    buildRepeatedString targetStr subStr = concat $ replicate (length targetStr `div` length subStr) subStr

isInvalid :: Integer -> Bool
isInvalid n = take l s == drop l s
  where
    l = length s `div` 2
    s = show n

parseRanges :: T.Text -> [(Integer, Integer)]
parseRanges input = parseAoCInput input rangesParser "rangesParser"
  where
    numParser = read <$> P.many1 P.digit
    rangeParser = (,) <$> (numParser <* P.char '-') <*> numParser
    rangesParser = P.sepBy1 rangeParser (P.char ',')
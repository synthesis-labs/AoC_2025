module AOC2025.Day02
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Integer
part1 input = sum $ concatMap countInvalids ranges
  where
    ranges = parseRanges input
    countInvalids (startN, endN) = filter isRepeatedTwice [startN .. endN]

part2 :: T.Text -> Integer
part2 input = sum $ concatMap countInvalids ranges
  where
    ranges = parseRanges input
    countInvalids (startN, endN) = filter isRepeatedN [startN .. endN]

isRepeatedN :: Integer -> Bool
isRepeatedN n = any (\factor -> concat (replicate (len `div` factor) (take factor s)) == s) factors
  where
    s = show n
    len = length s
    factors = [f | f <- [1 .. len `div` 2], len `mod` f == 0]

isRepeatedTwice :: Integer -> Bool
isRepeatedTwice n = take l s == drop l s
  where
    l = length s `div` 2
    s = show n

parseRanges :: T.Text -> [(Integer, Integer)]
parseRanges input = parseAoCInput input rangesParser "rangesParser"
  where
    numParser = read <$> P.many1 P.digit
    rangeParser = (,) <$> (numParser <* P.char '-') <*> numParser
    rangesParser = P.sepBy1 rangeParser (P.char ',')
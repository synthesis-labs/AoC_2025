module AOC2025.Day03
  ( part1,
    part2,
  )
where

import Data.Char (digitToInt)
import Data.Text qualified as T

part1 :: T.Text -> Integer
part1 input = sum $ foldr1 concatInts . buildNumber 2 [] <$> parseBatteries input

part2 :: T.Text -> Integer
part2 input = sum $ foldr1 concatInts . buildNumber 12 [] <$> parseBatteries input

type Battery = [Integer]

concatInts :: Integer -> Integer -> Integer
concatInts a b = a * 10 ^ length (show (abs b)) + b

buildNumber :: Int -> Battery -> Battery -> Battery
buildNumber 1 acc xs = acc ++ [maximum xs]
buildNumber numDigitsToFind acc xs = buildNumber (numDigitsToFind - 1) (acc ++ [nextMax]) restOfList
  where
    nextMax = maximum $ take (length xs - numDigitsToFind + 1) xs
    restOfList = drop 1 $ dropWhile (< nextMax) xs

parseBatteries :: T.Text -> [Battery]
parseBatteries input = fmap (fromIntegral . digitToInt) . T.unpack <$> T.lines input
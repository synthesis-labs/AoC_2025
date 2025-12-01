module AOC2025.Day01
  ( part1,
    part2,
  )
where

import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = fst $ count0s (parseRotations input) (0, 50)

part2 :: T.Text -> Int
part2 input = fst $ countClicks (parseRotations input) (0, 50)

data Rotation = L Int | R Int deriving (Show)

countClicks :: [Rotation] -> (Int, Int) -> (Int, Int)
countClicks [] acc = acc
countClicks (x : xs) (hits, dial) =
  case x of
    R rv ->
      if (dial + rv) < 100
        then countClicks xs (hits, dial + rv)
        else
          if (dial + rv) == 100
            then countClicks xs (hits + 1, 0)
            else countClicks xs (hits + (dial + rv) `div` 100, (dial + rv) `mod` 100)
    L rv ->
      if (dial - rv) > 0
        then countClicks xs (hits, dial - rv)
        else
          if (dial - rv) == 0
            then countClicks xs (hits + 1, 0)
            else
              if dial == 0
                then countClicks xs (hits + rv `div` 100, (dial - rv) `mod` 100)
                else countClicks xs (hits + 1 + abs ((dial - rv) `quot` 100), (dial - rv) `mod` 100)

count0s :: [Rotation] -> (Int, Int) -> (Int, Int)
count0s [] acc = acc
count0s (x : xs) (count, currentVal) =
  if newVal == 0
    then count0s xs (count + 1, newVal)
    else count0s xs (count, newVal)
  where
    newVal = (case x of L rv -> currentVal - rv; R rv -> currentVal + rv) `mod` 100

parseRotations :: T.Text -> [Rotation]
parseRotations input = parseAoCInput input rotationsParser "parseRotations"
  where
    numParser = read <$> P.many1 P.digit
    leftParser = L <$> (P.char 'L' *> numParser)
    rightParser = R <$> (P.char 'R' *> numParser)
    rotationParser = P.choice $ P.try <$> [leftParser, rightParser]
    rotationsParser = P.many1 $ rotationParser <* P.optional P.newline

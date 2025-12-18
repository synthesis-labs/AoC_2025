module AOC2025.Day05
  ( part1,
    part2,
  )
where

import Data.List (sort)
import Data.Text qualified as T
import Text.Parsec qualified as P
import Util.ParseUtils (parseAoCInput)

part1 :: T.Text -> Int
part1 input = foldr (\i acc -> if any (inRange i) ingredientRanges then acc + 1 else acc) 0 ingredients
  where
    inRange n (a, b) = n >= a && n <= b
    (ingredientRanges, ingredients) = parseIngredientDB input

part2 :: T.Text -> Integer
part2 input = foldr (\(a, b) acc -> acc + (b - a + 1)) 0 mergedRanges
  where
    mergedRanges = case ingredientRanges of [] -> []; (x : xs) -> mergeRanges xs [x]
    ingredientRanges = (\(a, _) -> sort a) $ parseIngredientDB input

type Min = Integer

type Max = Integer

type Ingredient = Integer

type IngredientRange = (Min, Max)

-- Turn into util
mergeRanges :: (Ord a) => [(a, a)] -> [(a, a)] -> [(a, a)]
mergeRanges [] acc = acc
mergeRanges ((cmin, cmax) : xs) acc =
  if cmin <= pmax
    then mergeRanges xs newAcc
    else mergeRanges xs $ acc ++ [(cmin, cmax)]
  where
    newAcc = take (length acc - 1) acc ++ [(pmin, newMax)]
    newMax = max cmax pmax
    (pmin, pmax) = last acc

parseIngredientDB :: T.Text -> ([IngredientRange], [Ingredient])
parseIngredientDB input = parseAoCInput input ingredientDBParser "ingredientDBParser"
  where
    numParser = read <$> P.many1 P.digit
    ingredientRangeParser = (,) <$> (numParser <* P.char '-') <*> numParser
    ingredientRangesParser = P.many1 $ ingredientRangeParser <* P.newline
    ingredientsParser = P.many1 $ numParser <* P.optional P.newline
    ingredientDBParser = (,) <$> (ingredientRangesParser <* P.newline) <*> ingredientsParser

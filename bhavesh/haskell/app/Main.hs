module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Text.Read (readMaybe)
import Util.AOCUtils (printDay, printYear, printYears)

main :: IO ()
main = do
  loadFile defaultConfig
  (currentYear, currentMonth) <- (\(x, y, _) -> (fromInteger x, y)) . toGregorian . utctDay <$> getCurrentTime
  let yearMax = if currentMonth < 12 then currentYear - 1 else currentYear
      yearPrompt = "Which year would you like to see?"
      yearErr = "Invalid year. Please enter a valid year between 2015 and " ++ show yearMax ++ "."
  putStrLn "Welcome to Advent of Code!"
  maybeYear <- prompt yearPrompt yearErr 2015 yearMax
  maybe (printYears yearMax) handleYear maybeYear

handleYear :: Int -> IO ()
handleYear year = do
  let dayPrompt = "Which day would you like to see?"
      dayErr = "Invalid day. Please enter a valid day between 1 and 25."
  maybeDay <- prompt dayPrompt dayErr 1 25
  maybe (printYear year) (handleDay year) maybeDay

handleDay :: Int -> Int -> IO ()
handleDay year day = do
  let aoCDay = (year, day)
  printDay aoCDay

prompt :: (Read a, Show a, Ord a) => String -> String -> a -> a -> IO (Maybe a)
prompt promptMsg errMsg minVal maxVal = do
  putStrLn $ promptMsg ++ " (" ++ show minVal ++ "-" ++ show maxVal ++ " or all):"
  input <- getLine
  case (input, readMaybe input) of
    ("all", _) -> pure Nothing
    (_, Just x) | x >= minVal && x <= maxVal -> pure (Just x)
    _ -> putStrLn errMsg >> prompt promptMsg errMsg minVal maxVal

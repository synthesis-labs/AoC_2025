module Util.AOCUtils
  ( printDay,
    printYear,
    printYears,
  )
where

import AOC2025.Module qualified as AOC2025
import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Time.Clock qualified as Clock
import GHC.IO (evaluate)
import Model
  ( AOCDayImpl (..),
    AOCDaySolution (..),
    AOCInputData,
    AOCResultStat,
    AOCResultStatRecord (..),
    AOCShow (aocShow),
    AOCYearDay,
    AOCYearDays,
  )
import Text.Printf (printf)
import Util.AOCHttpUtils (fetchData)
import Util.TimeUtils (formatNominalDiffTime)

printYears :: Int -> IO ()
printYears currentYear = mapM_ printYear [2015 .. currentYear]

printYear :: Int -> IO ()
printYear year = printTable True (year, [1 .. 25])

printDay :: AOCYearDay -> IO ()
printDay (year, day) = printTable True (year, [day])

printTable :: Bool -> AOCYearDays -> IO ()
printTable includeAnswers (year, days) = do
  (totalTime, stats) <- runAndGetStats (year, days)
  let (tableRows, afterTableRows) =
        foldr
          ( \stat@(ResultStatRecord _ p1v _ p2v _) (rows, after) ->
              if length p1v <= 23 && length p2v <= 23
                then (stat : rows, after)
                else
                  ( stat {p1SolValue = "Printed below", p2SolValue = "Printed below"} : rows,
                    stat : after
                  )
          )
          ([], [])
          stats
  printTableHeader totalTime
  forM_ tableRows printTableRow
  printTableFooter
  forM_ afterTableRows printAfterTableRow
  printf "\n"
  where
    printAfterTableRow :: AOCResultStatRecord -> IO ()
    printAfterTableRow (ResultStatRecord day p1v _ p2v _) = do
      printf "Day%02d-P1\n" day
      putStrLn p1v
      printf "Day%02d-P2\n" day
      putStr p2v

    printTableHeader :: Clock.NominalDiffTime -> IO ()
    printTableHeader totalTime =
      if includeAnswers
        then do
          printf "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n"
          printf "┃ Advent of Code                                                                %4d - Total time: %10s ┃\n" year (formatNominalDiffTime totalTime)
          printf "┣━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"
          printf "┃ Day ┃           Part 1 Answer ┃             Part 1 Time ┃           Part 2 Answer ┃             Part 2 Time ┃\n"
          printf "┣━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"
        else do
          printf "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n"
          printf "┃ Advent of Code            %4d - Total time: %10s ┃\n" year (formatNominalDiffTime totalTime)
          printf "┣━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┳━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"
          printf "┃ Day ┃             Part 1 Time ┃             Part 2 Time ┃\n"
          printf "┣━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━┫\n"

    printTableRow :: AOCResultStatRecord -> IO ()
    printTableRow (ResultStatRecord day p1v p1t p2v p2t) = do
      let t1 = formatNominalDiffTime p1t
      let t2 = formatNominalDiffTime p2t
      if includeAnswers
        then printf "┃ %3s ┃ %23s ┃ %23s ┃ %23s ┃ %23s ┃\n" (printf "%02d" day :: String) p1v t1 p2v t2
        else printf "┃ %3s ┃ %23s ┃ %23s ┃\n" (printf "%02d" day :: String) t1 t2

    printTableFooter :: IO ()
    printTableFooter =
      if includeAnswers
        then do
          printf "┗━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┛\n"
        else do
          printf "┗━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━┛\n"

runAndGetStats :: AOCYearDays -> IO AOCResultStat
runAndGetStats (year, days) = do
  totalTimeRef <- newIORef 0
  resultsRef <- newIORef []
  forM_ days $ \day -> do
    impl <- getParts (year, day)
    processImpl impl day totalTimeRef resultsRef
  totalTime <- readIORef totalTimeRef
  results <- readIORef resultsRef
  pure (totalTime, results)
  where
    mkRec = ResultStatRecord
    processImpl impl day ttRef resRef = case impl of
      AOCNoYear -> updateRefs (AOCDayNoSolution "") day ttRef resRef
      AOCNoDay -> updateRefs (AOCDayNoSolution "") day ttRef resRef
      _ -> do
        input <- fetchData (year, day)
        result <- benchImpl impl input
        updateRefs result day ttRef resRef
    updateRefs res day ttRef resRef = case res of
      AOCDayNoSolution _ ->
        modifyIORef' resRef (<> [mkRec day "" 0 "" 0])
      AOCDayPartSolution (r1, t1) -> do
        modifyIORef' ttRef (+ t1)
        modifyIORef' resRef (<> [mkRec day r1 t1 "" 0])
      AOCDayPartsSolution (r1, t1) (r2, t2) -> do
        modifyIORef' ttRef (+ (t1 + t2))
        modifyIORef' resRef (<> [mkRec day r1 t1 r2 t2])

benchImpl :: AOCDayImpl -> AOCInputData -> IO AOCDaySolution
benchImpl (AOCPartFunction part) input = do
  start <- Clock.getCurrentTime
  r <- evaluate (part input)
  end <- Clock.getCurrentTime
  pure $ AOCDayPartSolution (aocShow r, Clock.diffUTCTime end start)
benchImpl (AOCPartsFunction part1 part2) input = do
  s1 <- Clock.getCurrentTime
  r1 <- evaluate (part1 input)
  e1 <- Clock.getCurrentTime
  s2 <- Clock.getCurrentTime
  r2 <- evaluate (part2 input)
  e2 <- Clock.getCurrentTime
  pure $ AOCDayPartsSolution (aocShow r1, Clock.diffUTCTime e1 s1) (aocShow r2, Clock.diffUTCTime e2 s2)
benchImpl AOCNoDay _ = pure $ AOCDayNoSolution "This day is not yet implemented."
benchImpl AOCNoYear _ = pure $ AOCDayNoSolution "This year is not yet implemented."

getParts :: AOCYearDay -> IO AOCDayImpl
getParts (year, day) =
  pure $ case year of
    2025 -> AOC2025.getParts day
    _ -> AOCNoYear

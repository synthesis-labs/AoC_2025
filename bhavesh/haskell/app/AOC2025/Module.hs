module AOC2025.Module
  ( getParts,
  )
where

import AOC2025.Day01 qualified as Day01
import Model (AOCDayImpl (AOCNoDay, AOCPartsFunction))

getParts :: Int -> AOCDayImpl
getParts day =
  case day of
    01 -> AOCPartsFunction Day01.part1 Day01.part2
    _ -> AOCNoDay
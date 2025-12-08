module AOC2025.Module
  ( getParts,
  )
where

import AOC2025.Day01 qualified as Day01
import AOC2025.Day02 qualified as Day02
import AOC2025.Day03 qualified as Day03
import AOC2025.Day04 qualified as Day04
import AOC2025.Day05 qualified as Day05
import AOC2025.Day06 qualified as Day06
import Model (AOCDayImpl (AOCNoDay, AOCPartsFunction))

getParts :: Int -> AOCDayImpl
getParts day =
  case day of
    01 -> AOCPartsFunction Day01.part1 Day01.part2
    02 -> AOCPartsFunction Day02.part1 Day02.part2
    03 -> AOCPartsFunction Day03.part1 Day03.part2
    04 -> AOCPartsFunction Day04.part1 Day04.part2
    05 -> AOCPartsFunction Day05.part1 Day05.part2
    06 -> AOCPartsFunction Day06.part1 Day06.part2
    _ -> AOCNoDay
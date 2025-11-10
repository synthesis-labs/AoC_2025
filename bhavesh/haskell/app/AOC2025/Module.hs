module AOC2025.Module
  ( getParts,
  )
where

import Model (AOCDayImpl (AOCNoDay, AOCPartsFunction))

getParts :: Int -> AOCDayImpl
getParts day =
  case day of
    _ -> AOCNoDay
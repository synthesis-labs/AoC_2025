{-# LANGUAGE UndecidableInstances #-}

module Model
  ( AOCShow (..),
    AOCInputData,
    AOCYearDay,
    AOCYearDays,
    AOCDayImpl (..),
    AOCDaySolution (..),
    AOCResultStatRecord (..),
    AOCResultStat,
  )
where

import Data.Text qualified as T
import Data.Time.Clock (NominalDiffTime)

type ErrorMessage = String

type PartSolutionValue = String

type PartSolutionTime = NominalDiffTime

type TotalSolutionTime = NominalDiffTime

type Year = Int

type Day = Int

type PartSolution = (PartSolutionValue, PartSolutionTime)

--

class AOCShow a where
  aocShow :: a -> PartSolutionValue

instance AOCShow String where
  aocShow :: String -> PartSolutionValue
  aocShow = id

instance AOCShow T.Text where
  aocShow :: T.Text -> PartSolutionValue
  aocShow = T.unpack

instance {-# OVERLAPPABLE #-} (Show a) => AOCShow a where
  aocShow :: (Show a) => a -> PartSolutionValue
  aocShow = show

type AOCInputData = T.Text

type AOCYearDay = (Year, Day)

type AOCYearDays = (Year, [Day])

data AOCDayImpl
  = AOCNoDay
  | AOCNoYear
  | forall a. (AOCShow a) => AOCPartFunction (AOCInputData -> a)
  | forall a b. (AOCShow a, AOCShow b) => AOCPartsFunction (AOCInputData -> a) (AOCInputData -> b)

data AOCDaySolution
  = AOCDayNoSolution ErrorMessage
  | AOCDayPartSolution PartSolution
  | AOCDayPartsSolution PartSolution PartSolution

data AOCResultStatRecord = ResultStatRecord
  { day :: Day,
    p1SolValue :: PartSolutionValue,
    p1SolTime :: PartSolutionTime,
    p2SolValue :: PartSolutionValue,
    p2SolTime :: PartSolutionTime
  }
  deriving (Show)

type AOCResultStat = (TotalSolutionTime, [AOCResultStatRecord])

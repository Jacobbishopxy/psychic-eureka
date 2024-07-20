-- file: Util.hs
-- author: Jacob Xie
-- date: 2024/07/21 00:20:47 Sunday
-- brief:

module Util where

import Data.Time (DayOfMonth, MonthOfYear, UTCTime (..), Year, fromGregorian)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

defaultUTCTime :: UTCTime
defaultUTCTime = UTCTime (fromGregorian 2024 7 15) 0

mkUTCTime :: Year -> MonthOfYear -> DayOfMonth -> UTCTime
mkUTCTime yr mth dt = UTCTime (fromGregorian yr mth dt) 0

genUid :: IO UUID
genUid = nextRandom

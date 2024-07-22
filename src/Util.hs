{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- file: Util.hs
-- author: Jacob Xie
-- date: 2024/07/21 00:20:47 Sunday
-- brief:

module Util where

import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromJust)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (pack)
import Data.Time (DayOfMonth, MonthOfYear, UTCTime (..), Year, fromGregorian)
import Data.Typeable (Typeable)
import Data.UUID (UUID, fromString, fromText, toString)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (parseUrlPiece))

----------------------------------------------------------------------------------------------------
-- Helper
----------------------------------------------------------------------------------------------------

-- newtype id
newtype Id = Id UUID
  deriving
    ( Show,
      Eq,
      Ord,
      Read,
      Generic,
      Typeable,
      ToJSON,
      FromJSON,
      ToParamSchema,
      ToSchema
    )

instance FromHttpApiData Id where
  parseUrlPiece txt =
    case fromText txt of
      Just uuid -> Right (Id uuid)
      Nothing -> Left $ pack "Failed to parse Id"

idToString :: Id -> String
idToString (Id i) = toString i

defaultUTCTime :: UTCTime
defaultUTCTime = UTCTime (fromGregorian 2024 7 15) 0

mkUTCTime :: Year -> MonthOfYear -> DayOfMonth -> UTCTime
mkUTCTime yr mth dt = UTCTime (fromGregorian yr mth dt) 0

genUid :: IO UUID
genUid = nextRandom

mockId :: Id
mockId = Id $ fromJust $ fromString "123e4567-e89b-12d3-a456-426614174000"

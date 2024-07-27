{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- file: Util.hs
-- author: Jacob Xie
-- date: 2024/07/27 11:46:04 Saturday
-- brief:

module PsychicEureka.Util
  ( Id,
    id2str,
    str2id,
    genId,
    mockId,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Typeable)
import Data.Maybe (fromJust)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (pack)
import Data.UUID
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..))

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

id2str :: Id -> String
id2str (Id i) = toString i

str2id :: String -> Maybe Id
str2id s = Id <$> fromString s

genId :: IO Id
genId = Id <$> nextRandom

mockId :: Id
mockId = Id $ fromJust $ fromString "123e4567-e89b-12d3-a456-426614174000"

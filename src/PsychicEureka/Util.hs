{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
    getNowString,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (toStrict)
import Data.Data (Typeable)
import Data.Maybe (fromJust)
import Data.Swagger (ToParamSchema, ToSchema)
import Data.Text (pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Data.UUID (UUID, fromString, fromText, toByteString, toString, toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), MimeRender (mimeRender), PlainText, ToHttpApiData (..))
import Servant.Docs (ToSample (toSamples), singleSample)

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

instance ToHttpApiData Id where
  toUrlPiece (Id uuid) = toText uuid
  toHeader (Id uuid) = toStrict $ toByteString uuid
  toQueryParam (Id uuid) = toText uuid

instance ToSample Id where
  toSamples _ = singleSample $ Id (read "123e4567-e89b-12d3-a456-426614174000")

instance MimeRender PlainText Id where
  mimeRender _ (Id uuid) = toByteString uuid

----------------------------------------------------------------------------------------------------

id2str :: Id -> String
id2str (Id i) = toString i

str2id :: String -> Maybe Id
str2id s = Id <$> fromString s

genId :: IO Id
genId = Id <$> nextRandom

mockId :: Id
mockId = Id $ fromJust $ fromString "123e4567-e89b-12d3-a456-426614174000"

getNowString :: IO String
getNowString = getCurrentTime >>= return . formatTime defaultTimeLocale "%FT%T%QZ"

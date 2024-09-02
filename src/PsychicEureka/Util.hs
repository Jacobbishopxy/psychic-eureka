{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: Util.hs
-- author: Jacob Xie
-- date: 2024/07/27 11:46:04 Saturday
-- brief:

module PsychicEureka.Util
  ( Id,
    id2str,
    str2id,
    str2ids,
    str2strs,
    genId,
    mockId,
    mockUTCTime,
    getNowString,
  )
where

import Control.Lens ((&), (?~))
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (toStrict)
import Data.Data (Typeable)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, mapMaybe)
import Data.Swagger
import Data.Text (Text, pack)
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, getCurrentTime)
import Data.UUID (UUID, fromString, fromText, toByteString, toString, toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample (toSamples), singleSample)

----------------------------------------------------------------------------------------------------

-- | Newtype wrapper for UUID, representing a unique identifier.
newtype Id = Id UUID
  deriving
    ( Show,
      Eq,
      Ord,
      Read,
      Generic,
      Typeable,
      ToParamSchema
    )

-- | Convert 'Id' to JSON by converting the inner 'UUID' to text.
instance ToJSON Id where
  toJSON (Id uuid) = toJSON (toText uuid)

-- | Parse 'Id' from JSON by converting the text back to a 'UUID'.
instance FromJSON Id where
  parseJSON = withText "Id" $ \t ->
    case fromText t of
      Just uuid -> pure (Id uuid)
      Nothing -> fail "Invalid UUID string"

-- Implementing ToJSONKey for Id
instance ToJSONKey Id where
  toJSONKey = toJSONKeyText $ \(Id uuid) -> toText uuid

-- Implementing FromJSONKey for Id
instance FromJSONKey Id where
  fromJSONKey = FromJSONKeyTextParser parseId
    where
      parseId :: Text -> Parser Id
      parseId txt = case fromText txt of
        Just uuid -> pure (Id uuid)
        Nothing -> fail "Invalid UUID format"

-- | Swagger schema definition for 'Id', describing it as a string with a UUID format.
instance ToSchema Id where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Id") $
        mempty
          & type_ ?~ SwaggerString
          & format ?~ "uuid"
          & description ?~ "A unique identifier in UUID format"

-- | Parse 'Id' from a URL piece, handling HTTP API data.
instance FromHttpApiData Id where
  parseUrlPiece txt =
    case fromText txt of
      Just uuid -> Right (Id uuid)
      Nothing -> Left $ pack "Failed to parse Id"

-- | Convert 'Id' to various formats for use in HTTP APIs.
instance ToHttpApiData Id where
  toUrlPiece (Id uuid) = toText uuid
  toHeader (Id uuid) = toStrict $ toByteString uuid
  toQueryParam (Id uuid) = toText uuid

-- | Sample data for 'Id' used in testing.
instance ToSample Id where
  toSamples _ = singleSample $ Id (read "123e4567-e89b-12d3-a456-426614174000")

----------------------------------------------------------------------------------------------------

-- | Convert an 'Id' to a 'String' representation.
id2str :: Id -> String
id2str (Id i) = toString i

-- | Convert a 'String' to an 'Id', returning 'Nothing' if the string is not a valid UUID.
str2id :: String -> Maybe Id
str2id s = Id <$> fromString s

-- | Generate a new random 'Id'.
genId :: IO Id
genId = Id <$> nextRandom

-- | A mock 'Id' used for testing, with a predefined UUID.
mockId :: Id
mockId = Id $ fromJust $ fromString "123e4567-e89b-12d3-a456-426614174000"

-- | A mock 'UTCTime' representing a specific date and time, used for testing.
mockUTCTime :: UTCTime
mockUTCTime = UTCTime (fromGregorian 2024 7 15) 0

-- | Get the current time as a formatted string (ISO 8601 format).
getNowString :: IO String
getNowString = getCurrentTime <&> formatTime defaultTimeLocale "%FT%T%QZ"

-- | Convert a comma-separated 'String' into a list of 'Id's.
-- Invalid UUID strings will be ignored.
str2ids :: String -> [Id]
str2ids str = mapMaybe str2id names
  where
    -- Manually split the string by commas.
    names = splitByComma str

-- | Convert a comma-separated 'String' into a list of 'String's.
str2strs :: String -> [String]
str2strs = splitByComma

-- | Helper function to split a 'String' by commas.
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma s =
  let (nm, rest) = break (== ',') s
   in nm : case rest of
        [] -> []
        (_ : xs) -> splitByComma xs

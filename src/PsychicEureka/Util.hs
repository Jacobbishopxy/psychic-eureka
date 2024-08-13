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
import Data.Aeson (FromJSON (..), ToJSON (toJSON), withText)
import Data.ByteString (toStrict)
import Data.Data (Typeable)
import Data.Maybe (fromJust, mapMaybe)
import Data.Swagger (HasDescription (description), HasFormat (..), HasType (..), NamedSchema (..), SwaggerType (..), ToParamSchema, ToSchema (declareNamedSchema))
import Data.Text (pack)
import Data.Time (UTCTime (..), defaultTimeLocale, formatTime, fromGregorian, getCurrentTime)
import Data.UUID (UUID, fromString, fromText, toByteString, toString, toText)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Docs (ToSample (toSamples), singleSample)

----------------------------------------------------------------------------------------------------

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

instance ToJSON Id where
  toJSON (Id uuid) = toJSON (toText uuid)

instance FromJSON Id where
  parseJSON = withText "Id" $ \t ->
    case fromText t of
      Just uuid -> pure (Id uuid)
      Nothing -> fail "Invalid UUID string"

instance ToSchema Id where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Id") $
        mempty
          & type_ ?~ SwaggerString
          & format ?~ "uuid"
          & description ?~ "A unique identifier in UUID format"

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

----------------------------------------------------------------------------------------------------

id2str :: Id -> String
id2str (Id i) = toString i

str2id :: String -> Maybe Id
str2id s = Id <$> fromString s

genId :: IO Id
genId = Id <$> nextRandom

mockId :: Id
mockId = Id $ fromJust $ fromString "123e4567-e89b-12d3-a456-426614174000"

mockUTCTime :: UTCTime
mockUTCTime = UTCTime (fromGregorian 2024 7 15) 0

getNowString :: IO String
getNowString = getCurrentTime >>= return . formatTime defaultTimeLocale "%FT%T%QZ"

str2ids :: String -> [Id]
str2ids str = mapMaybe str2id names
  where
    -- Manually split the string by commas
    names = splitByComma str

str2strs :: String -> [String]
str2strs = splitByComma

-- Helper function to split a string by commas
splitByComma :: String -> [String]
splitByComma [] = []
splitByComma s =
  let (name, rest) = break (== ',') s
   in name : case rest of
        [] -> []
        (_ : xs) -> splitByComma xs

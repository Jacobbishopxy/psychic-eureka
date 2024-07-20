{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- file: Entities.hs
-- author: Jacob Xie
-- date: 2024/07/15 21:37:05 Monday
-- brief:

module Entities where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Persist (Entity (..), Id (..))
import Util

----------------------------------------------------------------------------------------------------
-- User
----------------------------------------------------------------------------------------------------

data UserData = UserData
  { name_ :: String,
    email_ :: String
  }
  deriving (Show, Generic)

instance FromJSON UserData where
  parseJSON = withObject "UserData" $ \v ->
    UserData
      <$> v .: "name"
      <*> v .: "email"

instance ToJSON UserData where
  toJSON (UserData n e) =
    object ["name" .= n, "email" .= e]

data User = User
  { userId :: Id,
    name :: String,
    email :: String,
    createdAt :: UTCTime,
    modifiedAt :: UTCTime
  }
  deriving (Show, Read, Generic, FromJSON, ToJSON)

instance Entity User where
  getId = userId
  mkCreatedAt u = do
    currentTime <- getCurrentTime
    return u {createdAt = currentTime, modifiedAt = currentTime}
  mkModifiedAt u = do
    currentTime <- getCurrentTime
    return u {modifiedAt = currentTime}

genUserFromData :: UserData -> IO User
genUserFromData ud = do
  uid <- genUid
  return $ User (Id uid) (name_ ud) (email_ ud) defaultUTCTime defaultUTCTime

modifyUserFromData :: UserData -> User -> User
modifyUserFromData ud u = u {name = name_ ud, email = email_ ud}

----------------------------------------------------------------------------------------------------

retrieveUser :: Id -> IO User
retrieveUser = retrieve

----------------------------------------------------------------------------------------------------
-- Posting
----------------------------------------------------------------------------------------------------

data Posting = Posting
  { postId :: Id,
    userRef :: Id,
    text :: String,
    postCreatedAt :: Maybe UTCTime,
    postModifiedAt :: Maybe UTCTime
  }
  deriving (Show, Read, Generic, FromJSON, ToJSON)

instance Entity Posting where
  getId = postId
  mkCreatedAt p = do
    currentTime <- getCurrentTime
    return p {postCreatedAt = Just currentTime, postModifiedAt = Just currentTime}
  mkModifiedAt p = do
    currentTime <- getCurrentTime
    return p {postModifiedAt = Just currentTime}

----------------------------------------------------------------------------------------------------

retrievePosting :: Id -> IO Posting
retrievePosting = retrieve

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- file: Entities.hs
-- author: Jacob Xie
-- date: 2024/07/15 21:37:05 Monday
-- brief:

module Entities where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Persist (Entity (..), Id)

data User = User
  { userId :: Id,
    name :: String,
    email :: String,
    createdAt :: Maybe UTCTime,
    modifiedAt :: Maybe UTCTime
  }
  deriving (Show, Read, Generic, FromJSON, ToJSON)

instance Entity User where
  getId = userId
  mkCreatedAt u = do
    currentTime <- getCurrentTime
    return u {createdAt = Just currentTime, modifiedAt = Just currentTime}
  mkModifiedAt u = do
    currentTime <- getCurrentTime
    return u {modifiedAt = Just currentTime}

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

retrieveUser :: Id -> IO User
retrieveUser = retrieve

retrievePosting :: Id -> IO Posting
retrievePosting = retrieve

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

-- file: Entities.hs
-- author: Jacob Xie
-- date: 2024/07/15 21:37:05 Monday
-- brief:

module Entities where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Persist (Entity (getId, retrieve), Id)

data User = User
  { userId :: Id,
    name :: String,
    email :: String
  }
  deriving (Show, Read, Generic, FromJSON, ToJSON)

instance Entity User where
  getId = userId

data Posting = Posting
  { postId :: Id,
    userRef :: Id,
    text :: String
  }
  deriving (Show, Read, Generic, FromJSON, ToJSON)

instance Entity Posting where
  getId = postId

retrieveUser :: Id -> IO User
retrieveUser = retrieve

retrievePosting :: Id -> IO Posting
retrievePosting = retrieve

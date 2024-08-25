{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- file: O2M.hs
-- author: Jacob Xie
-- date: 2024/08/14 08:53:13 Wednesday
-- brief:

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import PsychicEureka (Id, genId)
import qualified PsychicEureka.Biz as Biz
import qualified PsychicEureka.Cache as Cache
import PsychicEureka.Entity (Entity (..), NameEntity (getName))

----------------------------------------------------------------------------------------------------

data UserInput = UserInput
  { _user_name :: String
  }
  deriving (Generic, FromJSON, ToJSON)

data User = User
  { user_id :: Id,
    user_name :: String,
    user_created_time :: UTCTime,
    user_modified_time :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Cache.EntityCache)

data TodoInput = TodoInput
  { _todo_name :: String,
    _todo_detail :: String
  }
  deriving (Generic, FromJSON, ToJSON)

data Todo = Todo
  { todo_id :: Id,
    todo_name :: String,
    todo_detail :: String,
    todo_created_time :: UTCTime,
    todo_modified_time :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Cache.EntityCache)

----------------------------------------------------------------------------------------------------

instance NameEntity UserInput where
  getName = _user_name

instance NameEntity User where
  getName = user_name

instance Entity User where
  type EntityInput User = UserInput

  getId = user_id
  getCreatedTime = user_created_time
  getModifiedTime = user_modified_time
  createFromInput ui = do
    id' <- genId
    t <- getCurrentTime
    return $ User id' (_user_name ui) t t
  modifyFromInput ui u = do
    t <- getCurrentTime
    return $ u {user_name = _user_name ui, user_modified_time = t}

instance NameEntity TodoInput where
  getName = _todo_name

instance NameEntity Todo where
  getName = todo_name

instance Entity Todo where
  type EntityInput Todo = TodoInput
  getId = todo_id
  getCreatedTime = todo_created_time
  getModifiedTime = todo_modified_time
  createFromInput ti = do
    i <- genId
    t <- getCurrentTime
    return $ Todo i (_todo_name ti) (_todo_detail ti) t t
  modifyFromInput ti t = do
    t' <- getCurrentTime
    return $
      t
        { todo_name = _todo_name ti,
          todo_detail = _todo_detail ti,
          todo_modified_time = t'
        }

-- check this
instance Biz.OneToMany User Todo

----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  userStore <- Cache.initialize
  -- map1 <- Cache.getNameMap userStore
  -- print map1
  todoStore <- Cache.initialize
  -- map2 <- Cache.getNameMap todoStore
  -- print map2

  u <- Cache.save userStore (UserInput "Jacob")

  o2m <- Biz.construct userStore todoStore :: IO (Biz.CacheOneToMany User Todo)

  let userId = getId u

  _ <- Biz.saveRef o2m userId (TodoInput "grocery" "buy milk")

  refMap <- Biz.getRefMap o2m

  putStrLn "refMap: "
  print refMap

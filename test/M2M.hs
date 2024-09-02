{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- file: M2M.hs
-- author: Jacob Xie
-- date: 2024/09/02 23:15:49 Monday
-- brief:

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import PsychicEureka (Id, genId)
import qualified PsychicEureka.Biz.ManyToMany as M2M
import qualified PsychicEureka.Cache as Cache
import PsychicEureka.Entity (Entity (..), NameEntity (getName))

----------------------------------------------------------------------------------------------------

newtype AuthorInput = AuthorInput {_author_name :: String}
  deriving (Generic, FromJSON, ToJSON)

data Author = Author
  { author_id :: Id,
    author_name :: String,
    author_created_time :: UTCTime,
    author_modified_time :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Cache.EntityCache)

newtype LiteratureInput = LiteratureInput {_literature_name :: String}
  deriving (Generic, FromJSON, ToJSON)

data Literature = Literature
  { literature_id :: Id,
    literature_name :: String,
    literature_created_time :: UTCTime,
    literature_modified_time :: UTCTime
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, Cache.EntityCache)

----------------------------------------------------------------------------------------------------

instance NameEntity AuthorInput where
  getName = _author_name

instance NameEntity Author where
  getName = author_name

instance Entity Author where
  type EntityInput Author = AuthorInput
  getId = author_id
  getCreatedTime = author_created_time
  getModifiedTime = author_modified_time
  createFromInput ai = do
    id' <- genId
    t <- getCurrentTime
    return $ Author id' (_author_name ai) t t
  modifyFromInput ai a = do
    t <- getCurrentTime
    return $ a {author_name = _author_name ai, author_modified_time = t}

instance NameEntity LiteratureInput where
  getName = _literature_name

instance NameEntity Literature where
  getName = literature_name

instance Entity Literature where
  type EntityInput Literature = LiteratureInput
  getId = literature_id
  getCreatedTime = literature_created_time
  getModifiedTime = literature_modified_time
  createFromInput ai = do
    id' <- genId
    t <- getCurrentTime
    return $ Literature id' (_literature_name ai) t t
  modifyFromInput ai a = do
    t <- getCurrentTime
    return $ a {literature_name = _literature_name ai, literature_modified_time = t}

-- check this
instance M2M.ManyToMany Author Literature

----------------------------------------------------------------------------------------------------
-- Main
----------------------------------------------------------------------------------------------------

main :: IO ()
main = do
  -- init
  authorStore <- Cache.initialize
  literatureStore <- Cache.initialize

  a1 <- Cache.save authorStore (AuthorInput "Jacob")
  a2 <- Cache.save authorStore (AuthorInput "XY")
  a3 <- Cache.save authorStore (AuthorInput "Mia")

  let ai1 = getId a1
      ai2 = getId a2
      ai3 = getId a3

  m2m <- M2M.construct authorStore literatureStore :: IO (M2M.CacheManyToMany Author Literature)

  l1 <- M2M.saveRefL m2m ai1 (LiteratureInput "The Great Wall")
  let li1 = getId l1

  M2M.getRefMap m2m >>= print

  M2M.getManyRefL m2m ai1 [li1] >>= print

  l2 <- M2M.saveRefL m2m ai2 (LiteratureInput "The Yellow River")
  let li2 = getId l2

  M2M.bindRefL m2m ai3 li2 >>= print

  M2M.getRefMap m2m >>= print

  putStrLn "whatever"
